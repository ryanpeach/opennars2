import socket
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

# Create Socket
client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
print(address, port)
client_socket.connect((address, port))

# Main loop
while True:
    user = raw_input()
    data = user.split(' ')
    id0 = str(uuid.uuid4())
    data = [id0]+data
    if len(data) >= 2:
        if data[1] == 'quit':
            print("Quitting...")
            break
        data = IN.join(data)
        print("Sending: ", data)
        client_socket.sendall(data)
    else:
        print("Invalid")