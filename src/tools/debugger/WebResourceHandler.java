package tools.debugger;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;

import som.vm.NotYetImplementedException;

class WebResourceHandler implements HttpHandler {

  @Override
  public void handle(final HttpExchange exchange) throws IOException {
    WebDebugger.log("[REQ] " + exchange.getRequestURI().toString());
    String rootFolder = "/Users/smarr/Projects/SOM/SOMns/tools";
    String requestedFile = exchange.getRequestURI().toString();
    if (requestedFile.equals("/")) {
      requestedFile = "/index.html";
    }

    switch (requestedFile) {
      case "/index.html":
      case "/view.js":
      case "/vm-connection.js":
      case "/controller.js":
      case "/source.js":
      case "/visualizations.js":
        File f = new File(rootFolder + requestedFile);
        exchange.sendResponseHeaders(200, f.length());
        copy(f, exchange.getResponseBody());
        return;
      case "/favicon.ico":
        exchange.sendResponseHeaders(404, 0);
        exchange.close();
        return;
    }

    WebDebugger.log("[REQ] not yet implemented");
    throw new NotYetImplementedException();
  }

  private static void copy(final File f, final OutputStream out) throws IOException {
    byte[] buf = new byte[8192];

    InputStream in = new FileInputStream(f);

    int c = 0;
    while ((c = in.read(buf, 0, buf.length)) > 0) {
      out.write(buf, 0, c);
//        out.flush();
    }

    out.close();
    in.close();
  }
}
