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

/**
 * Utility methods for formatting.
 */
class Formatting {

    private Formatting() {}

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
