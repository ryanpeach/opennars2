import socket
import uuid
import argparse

QUIT = []
IN   = ":<:"
OUT  = ":>:"

# Argument Parsing
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

# Main Loop
while True:
    data = connection.recv(buffsz)
    
    # Check for lost connection
    if not data:
        print "Connection lost."
        break
    
    # Print and split received
    print("Received: " + str(data))
    data = str(data).split(IN)
    
    # Print labeled input
    if len(data) >= 2:
        print("ID: {}\nOP: {}".format(data[0], data[1]))
        if data[1] == 'quit':
            print("Quitting")    
            break
    # If not at least 2 elements, invalid input
    else:
        print("Invalid...")
        continue
    
    # Print args if they exist
    if len(data) > 2:
        print("Args: {}".format(data[2:]))
    
print("Shutting Down")