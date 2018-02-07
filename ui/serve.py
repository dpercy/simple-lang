#!/usr/bin/python

import SimpleHTTPServer
import SocketServer

PORT = 8000

Handler = SimpleHTTPServer.SimpleHTTPRequestHandler
Handler.extensions_map.update({
    '.mjs': 'application/javascript',
});

httpd = SocketServer.TCPServer(("", PORT), Handler)

print "Serving at http://localhost:"+str(PORT)+"/notebook.html"
httpd.serve_forever()
