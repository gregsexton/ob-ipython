from IPython.lib.kernel import find_connection_file
import IPython.kernel.blocking.client as client
import threading
import pprint
import json
import fileinput

cf = find_connection_file('18219')

c = client.BlockingKernelClient(connection_file=cf)
c.load_connection_file()
c.start_channels()

def printChannel(ch):
    while True:
        print(json.dumps(ch(), default=str))

chans = [c.get_iopub_msg, c.get_shell_msg, c.get_stdin_msg]

for ch in chans:
    t = threading.Thread(target=printChannel, args=(ch,))
    t.start()

for line in fileinput.input():
    j = json.loads(line)
    c.execute(j['execute']['code'])
