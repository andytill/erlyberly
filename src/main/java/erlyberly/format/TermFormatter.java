package erlyberly.format;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Formatter API for erlang terms, functions, exceptions, and everything
 * else which differs per language.
 */
public interface TermFormatter {

    String modFuncArgsToString(OtpErlangTuple mfa);

    String modFuncArityToString(OtpErlangTuple mfa);

    String exceptionToString(OtpErlangAtom errorClass, OtpErlangObject errorReason);

    StringBuilder appendToString(OtpErlangObject obj, StringBuilder sb);

    default String toString(OtpErlangObject obj) {
        return appendToString(obj, new StringBuilder()).toString();
    }

    String emptyTupleString();

    String tupleLeftParen();

    String tupleRightParen();

    String emptyListString();

    String listLeftParen();

    String listRightParen();
}
