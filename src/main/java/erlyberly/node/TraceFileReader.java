package erlyberly.node;

import java.io.File;
import java.io.FileInputStream;

import com.ericsson.otp.erlang.OtpInputStream;

public class TraceFileReader {

    public static void main(String[] args) throws Exception {
        byte[] bytes = new byte[1024 * 2];
        int read = 0;
        FileInputStream fs = new FileInputStream(new File("/tmp/trace0nonode@nohost"));
        try {
            while (true) {
                read = fs.read(bytes, 0, 1);
                if (read == -1)
                    return; // eof
                byte type = bytes[0];
                read = fs.read(bytes, 0, 4);
                int size = decode(bytes);
                read = fs.read(bytes, 0, size);
                if(read != size) {
                    System.out.println("ERROR: size of message was less than was actually read, size: " + size + " read: " + read);
                    return;
                }
                System.out.println("TYPE: " + type + " SIZE: " + size);
                OtpInputStream otpInputStream = new OtpInputStream(bytes, 0, size, 0);
                System.out.println("TRACE TERMS " + otpInputStream.read_any());
                otpInputStream.close();
            }
        }
        finally {
            fs.close();
        }
    }

    private static int decode(byte[] bytes) {
        return bytes[3] & 0xFF | (bytes[2] & 0xFF) << 8 | (bytes[1] & 0xFF) << 16 | (bytes[0] & 0xFF) << 24;
    }
}
