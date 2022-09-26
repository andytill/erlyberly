/**
 * erlyberly, erlang trace debugger
 * Copyright (C) 2016 Andy Till
 * <p>
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * <p>
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * <p>
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package erlyberly.format;

import com.ericsson.otp.erlang.*;
import erlyberly.node.OtpUtil;

import java.util.ArrayList;
import java.util.List;

public class LFEFormatter implements TermFormatter {

    /**
     * Convert an MFA tuple to a string, where the MFA must have the type:
     * <p>
     * {Module::atom(), Function::atom(), Args::[any()]}.
     */
    @Override
    public String modFuncArgsToString(final OtpErlangTuple mfa) {
        final StringBuilder sb = new StringBuilder();
        sb.append("(")
                .append(mfa.elementAt(0))
                .append(":")
                .append(mfa.elementAt(1));
        final OtpErlangList args = (OtpErlangList) mfa.elementAt(2);
        final List<String> stringArgs = new ArrayList<>();
        for (final OtpErlangObject arg : args) {
            stringArgs.add(this.toString(arg));
        }
        sb.append(String.join(", ", stringArgs))
                .append(")");
        return sb.toString();
    }

    @Override
    public String modFuncArityToString(final OtpErlangTuple mfa) {
        final StringBuilder sb = new StringBuilder();
        final OtpErlangList argsList = (OtpErlangList) mfa.elementAt(2);
        sb.append("(")
                .append(mfa.elementAt(0))
                .append(":")
                .append(mfa.elementAt(1))
                .append("/").append(argsList.arity())
                .append(")");
        return sb.toString();
    }

    @Override
    public String exceptionToString(final OtpErlangAtom errorClass, final OtpErlangObject errorReason) {
        return this.toString(errorClass) + ": " + this.toString(errorReason);
    }

    @Override
    public StringBuilder appendToString(final OtpErlangObject obj, final StringBuilder sb) {
        if (obj instanceof OtpErlangAtom) {
            sb.append("'").append(obj);
        } else if (obj instanceof OtpErlangBinary) {

            sb.append("#B(");
            Formatting.binaryToString((OtpErlangBinary) obj, " ", sb);
            sb.append(")");
        } else if (obj instanceof OtpErlangBitstr) {
            sb.append("#B(");
            Formatting.bitstringToString((OtpErlangBitstr) obj, " ", "(%d (size %d))", sb);
            sb.append(")");
        } else if (OtpUtil.isErlyberlyRecordField(obj)) {
            final OtpErlangObject fieldObj = ((OtpErlangTuple) obj).elementAt(2);
            this.appendToString(fieldObj, sb);
        } else if (obj instanceof OtpErlangTuple) {
            sb.append("#(");
            this.elementsToString(sb, ((OtpErlangTuple) obj).elements());
            sb.append(")");
        } else if (obj instanceof OtpErlangList) {
            sb.append("(");
            this.elementsToString(sb, ((OtpErlangList) obj).elements());
            if (!((OtpErlangList) obj).isProper()) {
                sb.append(this.cons());
                this.appendToString(((OtpErlangList) obj).getLastTail(), sb);
            }
            sb.append(")");
        } else if (obj instanceof OtpErlangString) {
            Formatting.appendString((OtpErlangString) obj, this, "\"", sb);
        } else {
            sb.append(obj.toString());
        }
        return sb;
    }

    private void elementsToString(final StringBuilder sb, final OtpErlangObject[] elements) {
        for (int i = 0; i < elements.length; i++) {
            if (0 != i) {
                sb.append(", ");
            }
            this.appendToString(elements[i], sb);
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
    public String mapLeft(final OtpErlangObject obj) {
        return "#M(";
    }

    @Override
    public String mapRight() {
        return ")";
    }

    @Override
    public Boolean isHiddenField(final OtpErlangObject key) {
        return false;
    }

    @Override
    public String cons() {
        return ".";
    }
}
