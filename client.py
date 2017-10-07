import jupyter_client as client
import threading
import json
import sys
import argparse

find_connection_file = client.find_connection_file

semaphore = threading.Semaphore(value=0)
interested_lock = threading.Lock()
interested = []

def msg_router(io, shell):
    while True:
        msg = io()
        msg['channel'] = 'io'
        msgid = msg['parent_header'].get('msg_id', None)
        with interested_lock:
            if msgid not in interested:
                continue
        print(json.dumps(msg, default=str))
        if (msg.get('msg_type', '') == 'status' and
            msg['content']['execution_state'] == 'idle'):
            break

    while True:
        msg = shell()
        msg['channel'] = 'shell'
        msgid = msg['parent_header'].get('msg_id', None)
        with interested_lock:
            if msgid not in interested:
                continue
        print(json.dumps(msg, default=str))
        if msg.get('msg_type', '') in ['execute_reply',
                                       'inspect_reply',
                                       'complete_reply']:
            semaphore.release()

def create_client(name):
    if name.endswith('.json'):
        cf = find_connection_file(name)
    else:
        cf = find_connection_file('emacs-' + name)
    c = client.BlockingKernelClient(connection_file=cf)
    c.load_connection_file()
    c.start_channels()
    io, shell = c.get_iopub_msg, c.get_shell_msg
    t = threading.Thread(target=msg_router, args=(io, shell))
    t.setDaemon(True)
    t.start()
    return c

parser = argparse.ArgumentParser()
parser.add_argument('--conn-file')
parser.add_argument('--execute', action='store_true')
parser.add_argument('--inspect', action='store_true')
parser.add_argument('--complete', action='store_true')
args = parser.parse_args()

c = create_client(args.conn_file)

with interested_lock:
    if args.execute:
        msgid = c.execute(sys.stdin.read(), allow_stdin=False)
        interested.append(msgid)

    elif args.inspect:
        req = json.loads(sys.stdin.read())
        code = req['code']
        msgid = c.inspect(code,
                          cursor_pos=req.get('pos', len(code)),
                          detail_level=req.get('detail', 0))
        interested.append(msgid)

    elif args.complete:
        req = json.loads(sys.stdin.read())
        code = req['code']
        pos = req.get('pos', len(code))
        # causes things to hang as kernel doesn't come back with a
        # complete_reply
        if code[pos-1] in ['\n', '\r']:
            sys.exit(0)
        msgid = c.complete(code,
                           cursor_pos=pos)
        interested.append(msgid)

semaphore.acquire()
