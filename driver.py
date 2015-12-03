try:                            # Jupyter and IPython >= 4.0
    import jupyter_client as client
    from jupyter_client import KernelManager
    find_connection_file = client.find_connection_file
    from jupyter_core.paths import jupyter_runtime_dir as runtime_dir
except ImportError:             # IPython 3
    from IPython.lib.kernel import find_connection_file
    import IPython.kernel.blocking.client as client
    from IPython.kernel.manager import KernelManager
    runtime_dir = None
    from IPython.utils.path import get_ipython_dir
    from IPython.core.profiledir import ProfileDir

import sys, signal, argparse, os.path
import threading, multiprocessing

import pprint
import json

import tornado
import tornado.web

# TODO: this is currently fragile, need to make this more robust. error
# handling around stuff, with proper http response, status code etc

handlers = {}
handlers_cond = threading.Condition()

def install_handler(msgid, handler):
    with handlers_cond:
        handlers[msgid] = handler
        handlers_cond.notify(n=3)

def remove_handler(msgid):
    with handlers_cond:
        del handlers[msgid]

def get_handler(msg):
    def ignore(msg): pass
    msgid = msg['parent_header'].get('msg_id', None)
    if not msgid:
        return ignore
    with handlers_cond:
        for i in range(20):
            if not handlers.has_key(msgid):
                handlers_cond.wait(timeout=0.05*i)
            else:
                break
        onmsg = handlers.get(msgid, ignore)
    return onmsg

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

def handler(webhandler, msgid, msg, msgs):
    msgs.append(msg)
    hasreply, hasidle = False, False
    for msg in msgs:
        if msg.get('msg_type', '') in ['execute_reply', 'inspect_reply']:
            hasreply = True
        elif (msg.get('msg_type', '') == 'status' and
              msg['content']['execution_state'] == 'idle'):
            hasidle = True
    if hasreply and hasidle:
        remove_handler(msgid)
        webhandler.set_header("Content-Type", "application/json")
        def accept(msg):
            return not msg['msg_type'] in ['status', 'execute_input']
        webhandler.write(json.dumps(filter(accept, msgs), default=str))
        webhandler.finish()

class ExecuteHandler(tornado.web.RequestHandler):
    @tornado.web.asynchronous
    def post(self, name):
        msgs = []
        c = get_client(name)
        msgid = c.execute(self.request.body.decode("utf-8"), allow_stdin=False)
        install_handler(msgid, lambda msg: handler(self, msgid, msg, msgs))

class InspectHandler(tornado.web.RequestHandler):
    @tornado.web.asynchronous
    def post(self, name):
        msgs = []
        req = json.loads(self.request.body.decode("utf-8"))
        code = req['code']
        c = get_client(name)
        msgid = c.inspect(code,
                          cursor_pos=req.get('pos', len(code)),
                          detail_level=req.get('detail', 0))
        install_handler(msgid, lambda msg: handler(self, msgid, msg, msgs))

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
    parser = argparse.ArgumentParser()
    parser.add_argument('--port', type=int)
    parser.add_argument('--kernel')
    parser.add_argument('--conn-file')
    args = parser.parse_args()
    if args.conn_file:
        if runtime_dir:
            conn_file = (args.conn_file if os.path.isabs(args.conn_file)
                         else os.path.join(runtime_dir(), args.conn_file))
        else: # IPython 3
            pd = ProfileDir.find_profile_dir_by_name(get_ipython_dir(), 'default')
            conn_file = os.path.join(pd.security_dir, args.conn_file)
        kwargs = {'connection_file': conn_file}
        if args.kernel:
            kwargs['kernel_name'] = args.kernel
        manager = KernelManager(**kwargs)

        semaphore = multiprocessing.Semaphore()
        semaphore.acquire()
        def onsignal(*args):
            semaphore.release()
        signal.signal(signal.SIGTERM, onsignal)
        import platform
        if platform.system() == 'Windows':
            signal.signal(signal.SIGBREAK, onsignal)
        else:
            signal.signal(signal.SIGQUIT, onsignal)
            # Emacs sends SIGHUP upon exit
            signal.signal(signal.SIGHUP, onsignal)

        manager.start_kernel()
        try:
            semaphore.acquire()
	except KeyboardInterrupt: pass
        manager.shutdown_kernel()
    else:
        app = make_app()
        app.listen(args.port)
        tornado.ioloop.IOLoop.current().start()

if __name__ == '__main__':
    main(sys.argv)
