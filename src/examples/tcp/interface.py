from narsocket import *
import uuid
import asyncore
from time import *
from Queue import *
from collections import namedtuple

class NARSOp():
    IN   = ":<:"
    OUT  = ":>:"
    def __init__(self, id0, opname, *args):
        self.id, self.op, self.args = id0, opname, args
    def inrep(self):
        return IN.join(iter(self))
    def outrep(self):
        return OUT.join(iter(self))
    def __str__(self):
        return self.outrep()
    def __iter__(self):
        return [self.id, self.op] + self.args

class OnlineNARS():
    # Constants
    CONFIRM = 'confirm'
    INVALID = 'invalid'
    RESERVED = set(['new-op','input','valid','concepts','concept','help','reset','quit','answer','say'])

    def __init__(self, host, port):
        self.client  = NARSocket(host,port)
        self.queue = Queue()
        self.ops = {}

    def __read(self, timeout = 5000):
        if self.queue.empty():
            data = self.client.get(timeout)
        else:
            data = self.queue.get(timeout = timeout)
        return self.process_input(data)

    def process_input(self, data):
        out = data.split(NARSOp.IN)
        return NARSOp(*out)

    def wait_confirm(self, id0, timeout = 5000):
        # Loop until you successfully read something (return), or timeout..
        start = time()
        stop = start + timeout
        while time() < stop:

            # Read data from client
            data = self.client.get(timeout = stop-time())

            # Which has a first element equal to id0
            temp = self.process_input(data)
            if temp.id == id0:

                # And whos second element is either confirm or invalid
                if temp.op in [self.CONFIRM, self.INVALID]:
                    return temp.op == self.CONFIRM  # Return true or false depending on value

                # We are expecting a confirmation statement, error if otherwise.
                else:
                    raise KeyError("Same id returned, expected a confirmation.")

            # If it is not the same id, save it
            else:
                self.queue.put(data)

        raise Empty("Confirmation timed out.")

    def wait_test(self, id0, opname, timeout = 5000):
        # Loop until you successfully read something (return), or timeout..
        start = time()
        stop = start + timeout
        while time() < stop:

            # Read data from client
            data = self.client.get(timeout = stop-time())

            # Which has a first element equal to id0
            temp = self.process_input(data)
            if temp.id == id0:

                # And whos second element is either confirm or invalid
                if temp.op == opname:
                    return temp.args  # Return output list

                # We are expecting a confirmation statement, error if otherwise.
                else:
                    raise KeyError("Same id returned, expected same opname.")

            # If it is not the same id, save it
            else:
                self.queue.put(data)

        raise Empty("Confirmation timed out.")

    def send(self, id0, op0, *args):
        self.client.put(NARSOp(id0, op0, *args).outrep())

    def newop(self, opname, f):
        if opname not in self.ops and opname not in self.RESERVED:
            self.ops[name] = f
            id0 = uuid4()
            self.send(id0, 'new-op', opname)
            return self.wait_confirm(id0)
        else:
            return False

    def answer(self, id0, success, *args):
        self.send(id0, 'answer', success, *args)
        return self.wait_confirm(id0, timeout = timeout)

    def sendNarsese(self, args, timeout = 5000):
        id0, opname = uuid4(), 'input'
        self.send(id0, opname, *args)
        return self.wait_test(id0, opname, timeout = timeout)

    def checkValid(self, args, timeout = 5000):
        id0, opname = uuid4(), 'valid'
        self.send(id0, opname, *args)
        return self.wait_test(id0, opname, timeout = timeout)

    def getConcepts(self, args = [], timeout = 5000):
        id0 = uuid4()
        if args == None or len(args) == 0:
            opname = 'concepts'
            self.send(id0, opname)
        else:
            opname = 'concept'
            self.send(id0, opname, *args)
        return self.wait_test(id0, opname, timeout = timeout)

    def getHelp(self, timeout = 5000):
        id0, opname = uuid4(), 'help'
        self.send(id0, opname)
        return self.wait_test(id0, opname, timeout = timeout)

    def reset(self, timeout = 5000):
        id0, opname = uuid4(), 'reset'
        self.send(id0, opname)
        return self.wait_conf(id0, timeout = timeout)

    def quit(self, timeout = 5000):
        id0, opname = uuid4(), 'quit'
        self.send(id0, opname)
        if self.wait_conf(id0, timeout = timeout):
            self.client.close()
        else:
            raise Exception("Could not quit.")

class NARS(OnlineNARS):
    def __init__(self, host, port, lein, path):
        pass
