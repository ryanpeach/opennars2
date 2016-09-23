import socket

class TimeoutError(socket.timeout):
    pass

none_val = lambda x: 0.0 if x is None else float(x)                         # Returns float(x), or 0 if x is None
