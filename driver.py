from IPython.lib.kernel import find_connection_file
import IPython.kernel.blocking.client as client

import threading

import pprint
import json

import tornado
import tornado.web

# TODO:
cf = find_connection_file('17715')

c = client.BlockingKernelClient(connection_file=cf)
c.load_connection_file()
c.start_channels()

# TODO: this is currently really fragile, need to make this much more robust
# error handling around stuff, with proper http response, status code etc

handlers = {}

def install_handlers(msgid, acc, finalizer):
    handlers[msgid] = (acc, finalizer)

def remove_handlers(msgid):
    del handlers[msgid]

def get_handler(msg):
    def ignore(msg): pass
    acc, final = handlers.get(msg['parent_header']['msg_id'], (ignore, ignore))
    msg_type = msg.get('msg_type', '')
    if msg_type == 'execute_reply':
        return final
    return acc

def msg_router(name, ch):
    while True:
        msg = ch()
        msg['channel'] = name
        print(json.dumps(msg, default=str))
        handler = get_handler(msg)
        handler(msg)

chans = [('io', c.get_iopub_msg), ('shell', c.get_shell_msg), ('stdin', c.get_stdin_msg)]
for name, ch in chans:
    t = threading.Thread(target=msg_router, args=(name, ch))
    t.start()

class ExecuteHandler(tornado.web.RequestHandler):
    @tornado.web.asynchronous
    def post(self):
        msgs = []
        def acc_msg(msg):
            msgs.append(msg)

        def finalize(msg):
            msgs.append(msg)
            remove_handlers(msgid)
            self.set_header("Content-Type", "application/json")
            self.write(json.dumps(msgs, default=str))
            self.finish()

        msgid = c.execute(self.request.body.decode("utf-8"))
        install_handlers(msgid, acc_msg, finalize)

class DebugHandler(tornado.web.RequestHandler):
    def get(self):
        self.write(json.dumps(handlers, default=str))

def make_app():
    return tornado.web.Application([
        # TODO: uri should take the kernel
        tornado.web.url(r"/execute", ExecuteHandler),
        tornado.web.url(r"/debug", DebugHandler),
        ])

def main():
    app = make_app()
    # TODO: port should be an arg
    app.listen(8888)
    tornado.ioloop.IOLoop.current().start()

if __name__ == '__main__':
    main()
