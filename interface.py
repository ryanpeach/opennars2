import socket
import uuid

QUIT = []
IN   = ":<:"
OUT  = ":>:"

def test_writer(address, timeout):
    client = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    client.connect((address, timeout))
    

class NARS():
    def __init__(self, address, timeout = 5000):
        self.address, self.timeout = address, timeout
        self.client = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.client.connect((self.address, self.timeout))
        
        # Setup reserved names
        self.reserved = set(["new-op", "answer", "input", "say"])
        self.ops = {}
        
    def send(self, id0, op0, *args):
        if id0 == None:
            id0 = uuid.uuid4()
        self.client.send(str(id0)+OUT+str(op0)+OUT.join(args))
        return id0
    
    def wait_confirm(self, id0):
        pass
    
    def newop(self, opname, f):
        if opname not in self.ops and opname not in self.reserved:
            self.ops[name] = f
            id0 = self.send(None, "new-op", opname)
            return self.wait_confirm(id0)
        else:
            return False
        
    def answer(self, opname, args0):
        if opname in self.ops:
            self.send(id0 "answer" self.ops[opname](args0))
            return self.wait_confirm(id0)
        else:
            return False
            
    def close(self):
        self.client.close()
        

def read_loop(self, in_buffer = 512):
    while True:
        # Read in new data
        data = self.client.recv(in_buffer)
        print "Received: " + data
        
        # Process data into list
        data = map(lambda s: s.strip(), data.split(IN))
        
        # Get id, op, and args
        if (len(data) == 2):
            id0, op0 = data
            args0 = []
        elif (len(data) > 2):
            id0, op0 = data[0], data[1]
            args0 = data[2:]
        else:
            self.close()
            raise Exception("Unable to parse: "+str(data))
        
        # Process operations
        if (op == "quit"):
            break;
        elif (op == "say"):
            print "Narjure says: " + str(data)
        elif (op in self.ops):
            self.answer(id0, op, args0)
    self.close()