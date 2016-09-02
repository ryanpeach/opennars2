from narsocket import *
from uuid import uuid4
import asyncore
from time import *
from Queue import *
from collections import namedtuple

class NARSOp():
    IN   = ":<:"
    OUT  = ":>:"
    def __init__(self, id0, opname, *args):
        self.id, self.op, self.args, self.t = id0, opname, args, time()
    def inrep(self):
        return IN.join(iter(self))
    def outrep(self):
        return OUT.join(iter(self))
    def __str__(self):
        return self.outrep()
    def __iter__(self):
        return [self.id, self.op] + self.args

class OnlineNARS(NARSocket):
    # Constants
    CONFIRM = 'confirm'
    INVALID = 'invalid'
    RESERVED = set(['new-op','input','valid','concepts','concept','help','reset','quit','answer','say'])

    def __init__(self, host, port, each_read = lambda: True):
        # Create the main callback function
        running = True
        def callback(msg):
            self.process_input(msg)
            return each_read()
            
        self.data = {}
        self.recvops = {'say':        print,
                        'answer':     lambda x: self.handle_answ(x),
                        'new-op':     lambda x: self.handle_info(x),
                        'input':      lambda x: self.handle_info(x),
                        'concept':    lambda x: self.handle_info(x),
                        'help':       print,
                        self.CONFIRM: lambda x: self.handle_info(x),
                        self.INVALID: lambda x: self.handle_info(x)
        }
        super(OnlineNARS, self).__init__(host, port, callback)

    def process_input(self, data):
        data = data.split(NARSOp.IN)
        if len(data) >= 2:
            out = NARSOp(*data)
            if out.op in self.ops:
                self.ops[out.op](out)
            else:
                self.error(-1,"Unknown Op.")
        self.error(-1, "Improper Input Format.")
        
    def error(self, id0, msg):
        self.buff(NARSOp(id0,self.INVALID,msg).outrep())

    def confirm(self, id0, *args):
        self.buff(NARSOp(id0,self.INVALID,*args).outrep())
        
    def send_op(self, op):
        self.buff(op.outrep())
        
    def handle_info(self, op):
        self.data[(op.id,op.op)] = op.args
    
    def handle_answ(self, op):
        self.data[op.args[0]] = op.args[1]
    
    def _ask(self, msg):
        """ Returns a future answer to the question. """
        if msg[-1] != '?':
            raise Exception("Message must end in a question mark.")
        valid = self.sendNarsese(msg)
        if not valid[0]:
            raise Exception("Message invalid.")
        return self.future(msg)
        
    def future(self, key):
        """ Returns a function which can be called to get the most recent version of the answer requested."""
        if key in self.query:
            raise Exception("Query already exists.")
        else:
            self.query[key] = None
        def wrapper():
            return self.query[key]
        def ready():
            return self.query[key] != None
        return wrapper, ready

    def newop(self, opname, f):
        # Create handler for function
        id0 = uuid4()
        def handle_op(op):
            # Try to run the function from the given operation
            try:
                args = f(op.args)
                success = True
            except Exception as e:
                success, args = False, [str(e)]
            
            # Send success / failure and post-conditions
            self.send_op(NARSOp(id0, 'answer', success, *args))
            
            # Return a future containing the confirmation or invalidity of the sent answer
            f1, f2 = self.future((id0, self.CONFIRM)), self.future((id0, self.INVALID))
            return lambda: f1() or f2()
            
        # Check if opname is already used
        if opname not in self.ops:
            self.ops[opname] = handle_op            # Put the handler in ops for callback
            notice = NARSOp(id0, 'new-op', opname)  # Tell NARS you have created the op
            self.send_op(notice)                    
            return self.future((id0, 'new-op'))     # Return a future confirmation
        else:
            raise Exception("Operation name already taken.")

    # ----------------------- Need to be changed ----------------
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
        return self.wait_confirm(id0, timeout = timeout)

    def quit(self, timeout = 5000):
        id0, opname = uuid4(), 'quit'
        self.send(id0, opname)
        if self.wait_confirm(id0, timeout = timeout):
            self.client.close()
        else:
            raise Exception("Could not quit.")

class NARS(OnlineNARS):
    def __init__(self, host, port, lein, path):
        pass
