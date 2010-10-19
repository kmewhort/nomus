package com.digitalpebble.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

/** 
 * Thread used to store the output ofan external process.
 **/

public class StreamReader implements Runnable {
  private static final int SIZE = 1024;
  private InputStreamReader is;
  private StringBuffer internalBuffer = new StringBuffer();

  public StreamReader(InputStream is) {
    this.is = new InputStreamReader(is);
  }
  
  public StreamReader(InputStream is, String charset) {
	this.is = new InputStreamReader(is);
  }

  public void run() {
    final char[] buf = new char[SIZE];
    int length;
    try {
      while((length = is.read(buf)) > 0) {
        // get a buffer of characters from this Stream
        internalBuffer.append(buf, 0, length);
      }
    } catch(Exception e) {
      // ignore errors
    } finally {
      try {
        is.close();
      } catch(IOException e) {
      }
    }
  }

  public StringBuffer getBuffer() {
    return this.internalBuffer;
  }
}
