from narsocket import *
import uuid
import argparse

QUIT = []
IN   = ":<:"
OUT  = ":>:"

parser = argparse.ArgumentParser(description='Process Server Parameters')
parser.add_argument('-a','--address', metavar='ADDRESS', type=str, default='localhost',
                   help='Address to connect to.')
parser.add_argument('-p','--port', metavar='PORT', type=int, default=8080,
                   help='Port for the connection.')
parser.add_argument('-b','--buffsz', metavar='BUFFER (bits)', type=int, default=512,
                   help='Buffer size.')
args = parser.parse_args()
address = args.address
port    = args.port
buffsz  = args.buffsz

# Connection
client = NARSocket(address, port)

# Test
client.send("1:<:input:<:<a-->b>.")
print(client.recv())
client.send("2:<:input:<:<b-->c>.")
print(client.recv())
client.send("3:<:input:<:<a-->c>?")
print(client.recv())

# Loop
while True:
    data = raw_input()
    if data == 'quit':
        print("Quitting...")
        break
    print("Sending: ", data)
    client.send(data)
    print(client.recv())
