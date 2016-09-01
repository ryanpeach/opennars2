import socket, time

class TimeoutError(Exception):
    pass

class NARSocket:
    '''REF:https://docs.python.org/2/howto/sockets.html#socket-howto
    '''
    def __init__(self, host, port):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.connect((host, port))

    def send(self, msg):
        totalsent = 0
        if len(msg) == 0 or msg[-1] != '\n':
            msg = msg + '\n'
        while totalsent < len(msg):
            sent = self.sock.send(msg[totalsent:])
            if sent == 0:
                raise RuntimeError("socket connection broken")
            totalsent = totalsent + sent

    def recv(self, timeout = 5000):
        out = ''
        start = time.time()
        stop = start + timeout
        while time.time() <= stop:
            chunk = self.sock.recv(8)
            if chunk == '':
                raise RuntimeError("socket connection broken")
            out += chunk
            if len(out) > 2:
                if out[-1:] == "\n":
                    return out[:-1]
        raise TimeoutError("Recieve timed out.")

    def close(self):
        pass
