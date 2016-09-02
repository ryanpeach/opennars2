import socket, time
#from Queue import *
#from multiprocessing import Pool

class TimeoutError(Exception):
    pass

class NARSocket(asyncore.dispatcher):
    '''REF:https://docs.python.org/2/howto/sockets.html#socket-howto
       REF:https://docs.python.org/2/library/asyncore.html
    '''
    END = '\n'
    E   = -(len(NARSocket.END)-1)
    def __init__(self, host, port, callback):
        asyncore.dispatcher.__init__(self)
        self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
        self.connect((host, port))
        self.read_log, self.write_log = [], []
        self.write_buffer = ''
        self.read_buffer = ''
        self.callback = callback
        
    def handle_connect(self):
        print "New Connection!"
    
    def handle_close(self):
        self.close()
    
    def readable(self):
        return True
        
    def handle_read(self):
        read = self.recv(8)
        if read == '':
            raise RuntimeError("socket connection broken")
        self.read_buffer += read
        if len(self.read_buffer) > 2:
            if self.read_buffer[self.E:] == self.END:
                out = self.read_buffer[:self.E]
                self.read_buffer = ''
                cont = self.read_callback(out)
                if not cont:
                    self.close()

    def writable(self):
        return (len(self.write_buffer) > 0)
        
    def handle_write(self):
        sent = self.send(self.write_buffer)
        if sent == 0:
            raise RuntimeError("socket connection broken")
        self.write_buffer = self.write_buffer[sent:]
        
    def buff(self, msg):
        if len(msg) == 0 or out[self.E:] == self.END:
            msg += '\n'
        self.write_buffer += msg
        self.write_log.append(msg)

    def read_callback(self, msg):
        self.read_log.append(msg)
        self.callback(msg)
