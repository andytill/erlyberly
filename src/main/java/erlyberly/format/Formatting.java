/**
 * erlyberly, erlang trace debugger
 * Copyright (C) 2016 Andy Till
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package erlyberly.format;

import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangBitstr;
import com.ericsson.otp.erlang.OtpErlangString;

/**
 * Utility methods for formatting.
 */
class Formatting {

    private Formatting() {}

    public static void appendString(OtpErlangString aString, TermFormatter formatter, String quote, StringBuilder sb) {
        String stringValue = aString.stringValue();
        // sometimes a list of integers can be mis-typed by jinterface as a string
        // in this case we format it ourselves as a list of ints
        boolean isString = isString(stringValue);
        if(isString) {
            sb.append(quote).append(stringValue.replace("\n", "\\n")).append(quote);
        }
        else {
            appendListOfInts(stringValue, formatter, sb);
        }
    }

    private static void appendListOfInts(String stringValue, TermFormatter formatter, StringBuilder sb) {
        sb.append(formatter.listLeftParen());
        for (int i = 0; i < stringValue.length(); i++) {
            int c = stringValue.charAt(i);
            sb.append(c);
            if(i < (stringValue.length() - 1)) {
                sb.append(",");
            }
        }
        sb.append(formatter.listRightParen());
    }

    private static boolean isString(String stringValue) {
        int length = stringValue.length();
        for (int i = 0; i < length; i++) {
            char c = stringValue.charAt(i);
            if(c < 10 || c > 127) {
                return false;
            }
        }
        return true;
    }

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

    public static void bitstringToString(OtpErlangBitstr bits, String sep,
                                         String bitsFormat, StringBuilder sb) {
        byte[] binValue = bits.binaryValue();
        for (int i=0; i < bits.size(); i++) {
            int b = binValue[i]>=0 ? binValue[i] : binValue[i]+256;
            sb.append(String.format("%d%s", b, sep));
        }
        int b = binValue[bits.size()];
        b = (b>=0 ? b : b+256) >> bits.pad_bits();
        sb.append(String.format(bitsFormat, b, 8-bits.pad_bits()));
    }

    private static boolean isDisplayableChar(int b) {
        return b > 31 && b < 127;
    }

    /**
     * Make a guess if the given binary is a UTF-8 string or not.
     */
    public static boolean isDisplayableString(OtpErlangBinary bin) {
        int expected;
        byte[] bytes = bin.binaryValue();
        for (int i = 0; i < bytes.length; i++) {
            int ch = bytes[i];
            if (isDisplayableChar(ch)) continue;
            else if ((ch & 0b11100000) == 0b11000000) expected = 1;
            else if ((ch & 0b11110000) == 0b11100000) expected = 2;
            else if ((ch & 0b11111000) == 0b11100000) expected = 3;
            else return false;

            while (expected > 0) {
                if (i + expected >= bytes.length) return false;
                if ((bytes[i + 1] & 0b11000000) != 0b10000000) return false;
                expected--;
                i++;
            }
        }
        return true;
    }
}
