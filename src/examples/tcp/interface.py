from narsocket import *
from uuid import uuid4
import asyncore
from time import *
from Queue import *
from collections import namedtuple
 
# Constants
IN   = ":<:"
OUT  = ":>:"
CONFIRM = 'confirm'
INVALID = 'invalid'
RESERVED = set(['new-op','input','valid','concepts','concept','help','reset','quit','answer','say'])

def istrue(msg):
    return msg in ['True', 'true', 't', 'T', '1', '1.' '1.0']

class NARSOp():
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
    def __init__(self, host = 'localhost', port = 8080):
        self.ops, self.query = {}, {}
        callback = lambda x: self.process_input(x)
        super(CommonNARS, self).__init__(host, port, callback)

    def error(self, id0, msg):
        self.buff(NARSOp(id0,self.INVALID,msg).outrep())

    def confirm(self, id0, *args):
        self.buff(NARSOp(id0,self.INVALID,*args).outrep())

    def process_input(self, data):
        data = data.split(NARSOp.IN)
        if len(data) >= 2:
            out = NARSOp(*data)
            if out.op in self.ops:
                self.ops[out.op](out)
            else:
                self.error(-1,"Unknown Op.")
        self.error(-1, "Improper Input Format.")

    def wait(self, key, timeout = 0):
        start = time()
        stop = start+timeout
        while time() < stop or timeout == 0:
            read = self.read().split(IN)
            data = NARSOp(*read)
            if key(data):
                return data.args
        raise TimeoutError("Wait confirmation timed out.")

    def wait_conf(self, id0, timeout = 0):
        start = time()
        stop = start+timeout
        while time() < stop or timeout == 0:
            read = self.read().split(IN)
            data = NARSOp(*read)
            if data.id == id0:
                if data.opname == CONFIRM:
                    return True
                elif data.opname == INVALID:
                    return False
        raise TimeoutError("Wait confirmation timed out.")

    def input_narsese(self, msgs, timeout = 0):
        if not isinstance(msgs, iter): [msgs]
        id0 = uuid4()
        self.buff(NARSOp(id0,'input',*msgs).outrep())
        out = map(istrue, self.wait(lambda d: d.id == id0 and d.op == 'valid', timeout))
        failed = [msg for tf, msg in zip(out,msgs) if not tf]
        if len(out)==1:
            return out[0]
        else:
            all(out), failed
            
    def valid_narsese(self, msgs, timeout = 0):
        if not isinstance(msgs, iter): [msgs]
        id0 = uuid4()
        self.buff(NARSOp(id0,'valid',*msgs).outrep())
        out = map(istrue, self.wait(lambda d: d.id == id0 and d.op == 'valid', timeout))
        failed = [msg for tf, msg in zip(out,msgs) if not tf]
        if len(out)==1:
            return out[0]
        else:
            all(out), failed
            
    def ask(self, msg, timeout = 0):
        if msg[-1] != '?':
            raise Exception("Message must end in a question mark.")
        valid = self.input_narsese(msg)
        if not valid[0]:
            raise Exception("Message invalid.")
        return self.wait(lambda d: d.op == 'answer' and d.args[0] == msg, timeout)[1:]

    def getConcepts(self, args = [], timeout = 0):
        id0 = uuid4()
        if args == None or len(args) == 0:
            opname = 'concept'
            self.buff(NARSOp(id0, opname).outrep())
        else:
            opname = 'concept'
            self.buff(NARSOp(id0, opname, *args).outrep())
        out = self.wait(lambda d: d.id == id0 and d.op == 'concept', timeout)
        test = [i == INVALID for i in out]
        return [i if good else None for i, good in zip(out, test)]

    def getHelp(self, timeout = 0):
        id0, opname = uuid4(), 'help'
        self.buff(NARSOp(id0, opname).outrep())
        return self.wait(lambda d: d.id == id0 and d.op == opname, timeout)

    def reset(self, timeout = 0):
        id0, opname = uuid4(), 'reset'
        self.buff(NARSOp(id0, opname).outrep())
        return self.wait_confirm(id0, timeout)

    def quit(self, timeout = 0):
        id0, opname = uuid4(), 'quit'
        self.buff(NARSOp(id0, opname).outrep())
        if self.wait_confirm(id0, timeout):
            self.close()
        else:
            raise Exception("Could not quit.")

from functools import partial
class NARSHost(OnlineNARS):
    def __init__(self, host, port, callbacks, each_read = lambda: True, rules = []):
        super(CommonNARS, self).__init__(host, port, each_read)
        for k, f in callbacks.iteritems(): # Initialize callbacks
            conf = self.new_op(k, f)
            if not conf: raise Exception("Invalid key: {}".format(k))
        conf = self.input_narsese(*rules)
        bad = [r for r, good in zip(rules, conf) if not good]
        if len(bad)>0: raise Exception("Invalid narsese: {}".format(bad))

    def new_op(self, opname, f):
        id0 = uuid4()
        self.ops[opname] = f
        self.buff(NARSOp(id0,'new-op',opname).outrep())
        return istrue(self.wait(id0, 'new-op')[0])
