try:                            # Jupyter and IPython >= 4.0
    import jupyter_client as client
    find_connection_file = client.find_connection_file
except ImportError:             # IPython 3
    from IPython.lib.kernel import find_connection_file
    import IPython.kernel.blocking.client as client

import sys
import threading

import pprint
import json

import tornado
import tornado.web

# TODO: this is currently fragile, need to make this more robust. error
# handling around stuff, with proper http response, status code etc

handlers = {}

def install_handlers(msgid, acc, finalizer):
    handlers[msgid] = (acc, finalizer)

def remove_handlers(msgid):
    del handlers[msgid]

def get_handler(msg):
    def ignore(msg): pass
    acc, final = handlers.get(msg['parent_header']['msg_id'], (ignore, ignore))
    msg_type = msg.get('msg_type', '')
    if msg_type in ['execute_reply', 'inspect_reply']:
        return final
    return acc

def msg_router(name, ch):
    while True:
        msg = ch()
        msg['channel'] = name
        handler = get_handler(msg)
        handler(msg)

clients = {}

def create_client(name):
    cf = find_connection_file('emacs-' + name)
    c = client.BlockingKernelClient(connection_file=cf)
    c.load_connection_file()
    c.start_channels()
    chans = [('io', c.get_iopub_msg), ('shell', c.get_shell_msg), ('stdin', c.get_stdin_msg)]
    for name, ch in chans:
        t = threading.Thread(target=msg_router, args=(name, ch))
        t.start()
    return c

def get_client(name):
    if name not in clients:
        clients[name] = create_client(name)
    return clients[name]

class ExecuteHandler(tornado.web.RequestHandler):
    @tornado.web.asynchronous
    def post(self, name):
        msgs = []
        def acc_msg(msg):
            msgs.append(msg)

        def finalize(msg):
            msgs.append(msg)
            remove_handlers(msgid)
            self.set_header("Content-Type", "application/json")
            self.write(json.dumps(msgs, default=str))
            self.finish()

        c = get_client(name)
        msgid = c.execute(self.request.body.decode("utf-8"), allow_stdin=False)
        install_handlers(msgid, acc_msg, finalize)

class InspectHandler(tornado.web.RequestHandler):
    @tornado.web.asynchronous
    def post(self, name):
        msgs = []
        def acc_msg(msg):
            msgs.append(msg)

        def finalize(msg):
            msgs.append(msg)
            remove_handlers(msgid)
            self.set_header("Content-Type", "application/json")
            self.write(json.dumps(msgs, default=str))
            self.finish()

        req = json.loads(self.request.body.decode("utf-8"))
        code = req['code']
        c = get_client(name)
        msgid = c.inspect(code,
                          cursor_pos=req.get('pos', len(code)),
                          detail_level=req.get('detail', 0))
        install_handlers(msgid, acc_msg, finalize)

class DebugHandler(tornado.web.RequestHandler):
    def get(self):
        self.write(json.dumps(clients, default=str))
        self.write(json.dumps(handlers, default=str))

def make_app():
    return tornado.web.Application([
        tornado.web.url(r"/execute/(\w+)", ExecuteHandler),
        tornado.web.url(r"/inspect/(\w+)", InspectHandler),
        tornado.web.url(r"/debug", DebugHandler),
        ])

def main(args):
    app = make_app()
    # TODO: parse args properly
    app.listen(args[1])
    tornado.ioloop.IOLoop.current().start()

if __name__ == '__main__':
    main(sys.argv)
