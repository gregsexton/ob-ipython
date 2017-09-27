import jupyter_client as client
import threading
import json
import sys
import argparse

find_connection_file = client.find_connection_file

semaphore = threading.Semaphore(value=0)
interested_lock = threading.Lock()
interested = []
hasreply = False
hasidle = False

def msg_router(name, ch):
    global hasreply, hasidle
    while True:
        msg = ch()
        msg['channel'] = name
        msgid = msg['parent_header'].get('msg_id', None)
        with interested_lock:
            if msgid not in interested:
                continue
        if msg.get('msg_type', '') in ['execute_reply', 'inspect_reply']:
            hasreply = True
        elif (msg.get('msg_type', '') == 'status' and
            msg['content']['execution_state'] == 'idle'):
            hasidle = True
        if not msg['msg_type'] in ['status', 'execute_input']:
            print(json.dumps(msg, default=str))
        if hasreply and hasidle:
            semaphore.release()

def create_client(name):
    if name.endswith('.json'):
        cf = find_connection_file(name)
    else:
        cf = find_connection_file('emacs-' + name)
    c = client.BlockingKernelClient(connection_file=cf)
    c.load_connection_file()
    c.start_channels()
    chans = [('io', c.get_iopub_msg), ('shell', c.get_shell_msg), ('stdin', c.get_stdin_msg)]
    for name, ch in chans:
        t = threading.Thread(target=msg_router, args=(name, ch))
        t.setDaemon(True)
        t.start()
    return c

parser = argparse.ArgumentParser()
parser.add_argument('--conn-file')
args = parser.parse_args()

c = create_client(args.conn_file)

with interested_lock:
    msgid = c.execute(sys.stdin.read(), allow_stdin=False)
    interested.append(msgid)

semaphore.acquire()
