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

import java.util.ArrayList;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.node.OtpUtil;

public class LFEFormatter implements TermFormatter {

    /**
     * Convert an MFA tuple to a string, where the MFA must have the type:
     *
     * {Module::atom(), Function::atom(), Args::[any()]}.
     */
    @Override
    public String modFuncArgsToString(OtpErlangTuple mfa) {
        StringBuilder sb = new StringBuilder();
        sb.append("(")
          .append(mfa.elementAt(0))
          .append(":")
          .append(mfa.elementAt(1));
        OtpErlangList args = (OtpErlangList) mfa.elementAt(2);
        ArrayList<String> stringArgs = new ArrayList<>();
        for (OtpErlangObject arg : args) {
            stringArgs.add(toString(arg));
        }
        sb.append(String.join(", ", stringArgs))
          .append(")");
        return sb.toString();
    }

    @Override
    public String modFuncArityToString(OtpErlangTuple mfa) {
        StringBuilder sb = new StringBuilder();
        OtpErlangList argsList = (OtpErlangList) mfa.elementAt(2);
        sb.append("(")
          .append(mfa.elementAt(0))
          .append(":")
          .append(mfa.elementAt(1))
          .append("/").append(argsList.arity())
          .append(")");
        return sb.toString();
    }

    @Override
    public String exceptionToString(OtpErlangAtom errorClass, OtpErlangObject errorReason) {
        return toString(errorClass) + ": " + toString(errorReason);
    }

    @Override
    public StringBuilder appendToString(OtpErlangObject obj, StringBuilder sb) {
        if(obj instanceof OtpErlangAtom) {
            sb.append("'").append(obj.toString());
        }
        else if(obj instanceof OtpErlangBinary) {

            sb.append("#B(");
            Formatting.binaryToString((OtpErlangBinary) obj, " ", sb);
            sb.append(")");
        }
        else if(OtpUtil.isErlyberlyRecordField(obj)) {
            OtpErlangObject fieldObj = ((OtpErlangTuple)obj).elementAt(2);
            appendToString(fieldObj, sb);
        }
        else if(obj instanceof OtpErlangTuple) {
            sb.append("#(");
            elementsToString(sb, ((OtpErlangTuple) obj).elements());
            sb.append(")");
        }
        else if(obj instanceof OtpErlangList) {
            sb.append("(");
            elementsToString(sb, ((OtpErlangList) obj).elements());
            if(!((OtpErlangList) obj).isProper()) {
                sb.append(cons());
                appendToString(((OtpErlangList) obj).getLastTail(), sb);
            }
            sb.append(")");
        }
        else if(obj instanceof OtpErlangString) {
            Formatting.appendString((OtpErlangString) obj, this, "\"", sb);
        }
        else {
            sb.append(obj.toString());
        }
        return sb;
    }

    private void elementsToString(StringBuilder sb, OtpErlangObject[] elements) {
        for(int i=0; i < elements.length; i++) {
            if(i != 0) {
                sb.append(", ");
            }
            appendToString(elements[i], sb);
        }
    }

    @Override
    public String emptyTupleString() {
        return "#( )";
    }

    @Override
    public String tupleLeftParen() {
        return "#(";
    }

    @Override
    public String tupleRightParen() {
        return ")";
    }

    @Override
    public String emptyListString() {
        return "( )";
    }

    @Override
    public String listLeftParen() {
        return "(";
    }

    @Override
    public String listRightParen() {
        return ")";
    }

    @Override
    public String mapLeft(OtpErlangObject obj) {
        return "#M(";
    }

    @Override
    public String mapRight() {
        return ")";
    }

    @Override
    public Boolean isHiddenField(OtpErlangObject key) {return false;}

    @Override
    public String cons() {
        return ".";
    }
}
