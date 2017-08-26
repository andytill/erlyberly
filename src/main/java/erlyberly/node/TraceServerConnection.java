package erlyberly.node;

import java.io.BufferedInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.Socket;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpInputStream;

public class TraceServerConnection {

    private static final int DEFAULT_BUFFER_SIZE = 1024*10;

    private final TraceManager traceManager;

    private Socket socket;
    private Thread thread;
    private byte[] buffer;

    public TraceServerConnection(TraceManager aTraceManager) {
        this.traceManager = aTraceManager;
    }

    public void connect(String host, int port) throws IOException {
        socket = new Socket(host, port);
        thread = new Thread(this::run, "Tracer TCP Connection");
        thread.start();
    }

    private void run() {
        BufferedInputStream inStream;
        FileOutputStream outStream = null;
        buffer = new byte[DEFAULT_BUFFER_SIZE];
        int read = 0;
        try {
            inStream = new BufferedInputStream(socket.getInputStream());
            outStream = new FileOutputStream("/tmp/erlyberlytrace");
            int offset = 0;
            while(true) {
                // offset in the byte array to write to
                offset = 0;
                // read the message type, read blocks but if the read size if less
                // than 1 then just loop again, docs are a little unclear as to what
                // circumstance it will ceast to block
                read = inStream.read(buffer, offset++, 1);
                if(read < 1) {
                    continue;
                }
                // read the size of the binary term
                read = inStream.read(buffer, offset, 4);
                // TODO better check read value is 4
                assert read == 4 : read;
                // if the size of the message is greater than the buffer then
                // permanently resize the buffer to the size of the message
                int size = decodeIntFromBytes(buffer, offset);
                offset += 4;
                if(size > buffer.length) {
                    buffer = new byte[5+size];
                }
                read = inStream.read(buffer, offset, size);
                assert read == size : "read: " + read + " size: " + size;
                OtpInputStream otpInputStream = new OtpInputStream(buffer, offset, size, 0);
                OtpErlangObject traceTerm = otpInputStream.read_any();
                assert traceTerm instanceof OtpErlangTuple : traceTerm;
                traceManager.collateTraceSingle((OtpErlangTuple) traceTerm);
                otpInputStream.close();
                System.out.println(traceTerm); // TODO remove
                outStream.write(buffer, 0, 5+size);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        finally {
            safeCloseStream(outStream);
        }
    }

    private void safeCloseStream(FileOutputStream outStream) {
        try {
            outStream.close();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static int decodeIntFromBytes(byte[] bytes, int offset) {
        return bytes[3+offset] & 0xFF | (bytes[2+offset] & 0xFF) << 8 | (bytes[1+offset] & 0xFF) << 16 | (bytes[0+offset] & 0xFF) << 24;
    }
}
