package erlyberly;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * A wrapper class around a {@link TermTreeView} tree item, with a erlang term
 * and the string which gets shown in the tree. The string is not necessarily
 * derivable from the term, and depends on its position in the tree so much be
 * separated.
 */
class TermTreeItem {
    private final String toString;
    private final OtpErlangObject object;
    public TermTreeItem(OtpErlangObject object, String toString) {
        this.object = object;
        this.toString = toString;
    }
    public String getToString() {
        return toString;
    }
    public OtpErlangObject getObject() {
        return object;
    }
    @Override
    public String toString() {
        return toString;
    }
}
