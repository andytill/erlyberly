package erlyberly.format;

import com.ericsson.otp.erlang.OtpErlangBinary;

/**
 * Utility methods for formatting.
 */
class Formatting {

    /**
     * Append the binary term bytes to a string builder. It attempts to display character data
     * as strings.
     */
    public static void binaryToString(OtpErlangBinary bin, String sep, StringBuilder sb) {
        int length = sb.length();
        boolean inString = false;

        for (int b : bin.binaryValue()) {
            if(isDisplayableChar(b)) {
                if(!inString) {
                    if((sb.length() - length) > 0) {
                        sb.append(sep);
                    }

                    sb.append("\"");
                }
                inString = true;
                sb.append((char)b);
            }
            else {
                if(inString) {
                    sb.append("\"");
                    inString = false;
                }

                if((sb.length() - length) > 0) {
                    sb.append(sep);
                }

                if(b < 0) {
                    b = 256 + b;
                }
                sb.append(Integer.toString(b));
            }
        }

        if(inString) {
            sb.append("\"");
        }
    }

    private static boolean isDisplayableChar(int b) {
        return b > 31 && b < 127;
    }
}
