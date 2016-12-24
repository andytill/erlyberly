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
import com.ericsson.otp.erlang.OtpErlangString;

/**
 * Utility methods for formatting.
 */
class Formatting {

    private Formatting() {}

    public static void appendString(OtpErlangString aString, TermFormatter formatter, StringBuilder sb) {
        String stringValue = aString.stringValue();
        // sometimes a list of integers can be mis-typed by jinterface as a string
        // in this case we format it ourselves as a list of ints
        boolean isString = isString(stringValue);
        if(isString) {
            sb.append("\"").append(stringValue.replace("\n", "\\n")).append("\"");
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

    private static boolean isDisplayableChar(int b) {
        return b > 31 && b < 127;
    }
}
