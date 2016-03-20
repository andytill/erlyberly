package erlyberly.format;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * 
 */
public interface TermFormatter {

    String mfaToString(OtpErlangTuple mfa);

    String exceptionToString(OtpErlangAtom errorClass, OtpErlangObject errorReason);

    String toString(OtpErlangObject obj);

    default StringBuilder appendToString(OtpErlangObject obj, StringBuilder sb) {
        sb.append(toString(obj));
        return sb;
    }

    String emptyTupleString();

    String tupleLeftParen();

    String tupleRightParen();

    String emptyListString();

    String listLeftParen();

    String listRightParen();

    String modFuncArityToString(OtpErlangTuple functionFromMap);
}
