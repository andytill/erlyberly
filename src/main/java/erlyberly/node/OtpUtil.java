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
package erlyberly.node;

import com.ericsson.otp.erlang.*;

import java.io.IOException;
import java.util.*;

/**
 * Sin bin for utils dealing with jinterface.
 */
public enum OtpUtil {
    ;

    private static final OtpErlangAtom ERLYBERLY_RECORD_ATOM = OtpUtil.atom("erlyberly_record");

    private static final OtpErlangAtom ERLYBERLY_RECORD_FIELD_ATOM = OtpUtil.atom("erlyberly_record_field");

    private static final Set<Class<?>> CONTAINER_TERM_TYPES = new HashSet<>(Arrays.asList(OtpErlangTuple.class, OtpErlangMap.class, OtpErlangList.class));


    private static final Set<Class<?>> LARGE_TERM_TYPES = new HashSet<>(Arrays.asList(OtpErlangFun.class, OtpErlangExternalFun.class));

    private static final OtpErlangAtom TRUE_ATOM = new OtpErlangAtom(true);
    private static final OtpErlangAtom FALSE_ATOM = new OtpErlangAtom(false);
    private static final OtpErlangAtom CALL_ATOM = new OtpErlangAtom("call");
    private static final OtpErlangAtom USER_ATOM = new OtpErlangAtom("user");
    private static final OtpErlangAtom ERROR_ATOM = atom("error");
    public static final OtpErlangAtom OK_ATOM = atom("ok");

    public static OtpErlangTuple tuple(final Object... elements) {
        final OtpErlangObject[] tuple = toOtpElementArray(elements);

        return new OtpErlangTuple(tuple);
    }

    public static OtpErlangList list(final Object... elements) {
        final OtpErlangObject[] tuple = toOtpElementArray(elements);

        return new OtpErlangList(tuple);
    }

    private static OtpErlangObject[] toOtpElementArray(final Object... elements) {
        final OtpErlangObject[] tuple = new OtpErlangObject[elements.length];

        for (int i = 0; i < elements.length; i++) {
            final Object e = elements[i];
            if (e instanceof Integer) {
                tuple[i] = new OtpErlangLong(((Integer) e).longValue());
            } else if (e instanceof Long) {
                tuple[i] = new OtpErlangLong(((Long) e).longValue());
            } else if (e instanceof OtpErlangObject) {
                tuple[i] = (OtpErlangObject) e;
            } else if (e instanceof String) {
                tuple[i] = new OtpErlangString((String) e);
            } else if (e instanceof Boolean) {
                if (Boolean.TRUE.equals(e)) tuple[i] = TRUE_ATOM;
                else tuple[i] = FALSE_ATOM;
            } else {
                throw new RuntimeException(e + " cannot be converted to an OtpErlangObject");
            }
        }
        return tuple;
    }


    public static OtpErlangAtom atom(final String name) {
        return new OtpErlangAtom(name.intern());
    }

    /**
     * Take an {@link OtpErlangList} of erlang key value tuples and converts it to a map.
     */
    public static Map<Object, Object> propsToMap(final OtpErlangList pinfo) {
        final Map<Object, Object> map = new HashMap<>();
        for (final OtpErlangObject otpErlangObject : pinfo) {
            if (otpErlangObject instanceof OtpErlangTuple && 2 == ((OtpErlangTuple) otpErlangObject).arity()) {
                final OtpErlangTuple tuple = ((OtpErlangTuple) otpErlangObject);
                map.put(tuple.elementAt(0), tuple.elementAt(1));
            }
        }
        return map;
    }

    public static OtpErlangObject[] elementsForTerm(final OtpErlangObject obj) {
        assert null != obj;

        if (obj instanceof OtpErlangTuple) return ((OtpErlangTuple) obj).elements();
        else if (obj instanceof OtpErlangList) return ((OtpErlangList) obj).elements();
        else throw new RuntimeException("No brackets for type " + obj.getClass());
    }


    public static boolean isTupleTagged(final OtpErlangObject tag, final OtpErlangObject result) {
        return isTupleTagged(tag, 0, result);
    }

    private static boolean isTupleTagged(final OtpErlangObject tag, final int index, final OtpErlangObject result) {
        boolean r = false;

        if (result instanceof OtpErlangTuple) {
            final OtpErlangTuple resultTuple = (OtpErlangTuple) result;
            r = 0 < resultTuple.arity() && resultTuple.elementAt(0).equals(tag);
        }

        return r;
    }

    /**
     * Checks if a term is tuple tagged with the erlyberly_record atom, meaning
     * it has metadata tagged with record and field names around the field values.
     */
    public static boolean isErlyberlyRecord(final OtpErlangObject obj) {
        return isTupleTagged(ERLYBERLY_RECORD_ATOM, obj);
    }

    public static boolean isErlyberlyRecordField(final OtpErlangObject obj) {
        return isTupleTagged(ERLYBERLY_RECORD_FIELD_ATOM, obj);
    }

    public static boolean isErrorReason(final OtpErlangObject reason, final OtpErlangObject error) {
        assert isTupleTagged(ERROR_ATOM, error) : "tuple " + error + "is not tagged with 'error'";
        return isTupleTagged(reason, 1, error);
    }

    public static OtpErlangList toOtpList(final OtpErlangObject obj) {
        if (obj instanceof OtpErlangList) {
            return (OtpErlangList) obj;
        } else if (obj instanceof OtpErlangString) {
            final OtpErlangString s = (OtpErlangString) obj;

            return new OtpErlangList(s.stringValue());
        } else {
            throw new ClassCastException(obj + " cannot be converted to an OtpErlangList");
        }
    }

    public static void sendRPC(final OtpConn conn, final OtpMbox m, final OtpErlangAtom mod, final OtpErlangAtom fun, final OtpErlangList args) throws IOException {
        final OtpErlangTuple rpcMessage = tuple(m.self(), tuple(CALL_ATOM, mod, fun, args, USER_ATOM));

        conn.send(m.self(), "rex", rpcMessage);
    }

    public static OtpErlangObject tupleElement(final int i, final OtpErlangObject obj) {
        return ((OtpErlangTuple) obj).elementAt(i);
    }

    public static OtpErlangObject[] iterableElements(final OtpErlangObject obj) {
        if (obj instanceof OtpErlangTuple) return ((OtpErlangTuple) obj).elements();
        else if (obj instanceof OtpErlangList) return ((OtpErlangList) obj).elements();
        else if (obj instanceof OtpErlangString) {
            final OtpErlangString s = (OtpErlangString) obj;
            return new OtpErlangList(s.stringValue()).elements();
        } else if (obj instanceof OtpErlangMap) {
            final OtpErlangMap m = (OtpErlangMap) obj;
            return new OtpErlangList(m.keys()).elements();
        } else {
            throw new RuntimeException(obj + " cannot return OtpErlangObject[]");
        }
    }

    /**
     * A short term can be displayed on a single line and does not have to be
     * broken down further.
     */
    public static boolean isLittleTerm(final OtpErlangObject obj) {
        final OtpErlangObject[] elements;

        if (obj instanceof OtpErlangList) {
            // if the list is empty consider it a little term
            return 0 == ((OtpErlangList) obj).arity();
        } else if (LARGE_TERM_TYPES.contains(obj.getClass())) {
            return false;
        } else if (CONTAINER_TERM_TYPES.contains(obj.getClass())) {
            elements = iterableElements(obj);
            // short lists and tuples which do not contain other short lists or
            // tuples are ok
            if (3 < elements.length) return false;
        } else {
            return true;
        }

        for (final OtpErlangObject e : elements) {
            if (LARGE_TERM_TYPES.contains(e.getClass()) || CONTAINER_TERM_TYPES.contains(e.getClass())) {
                return false;
            } else if (e instanceof OtpErlangString) {
                final int stringLength = ((OtpErlangString) e).stringValue().length();
                if (50 < stringLength) return true;
            } else if (e instanceof OtpErlangBinary) {
                final int binaryLength = ((OtpErlangBitstr) e).size();
                if (50 < binaryLength) return true;
            }

        }
        return true;
    }

    /**
     * jinterface interprets lists of integers OtpErlangString whatever
     * might be the intent, for example a list of function arguments `[10]`.
     * <p>
     * This can cause ClassCastExceptions when something that is normally an
     * OtpErlangList comes back as an OtpErlangString, which does not inherit
     * from OtpErlangList!
     */
    public static OtpErlangList toErlangList(final OtpErlangObject obj) {
        if (obj instanceof OtpErlangString) {
            return new OtpErlangList(((OtpErlangString) obj).stringValue());
        } else {
            // we have done our best to convert the nobbly string objects to
            // lists, if it fails just throw a ClassCastException
            return (OtpErlangList) obj;
        }
    }

    public static OtpErlangTuple toTuple(final OtpErlangObject obj) {
        if (!(obj instanceof OtpErlangTuple)) {
            throw new ClassCastException(obj + " cannot be case to OtpErlangTuple.");
        }
        return (OtpErlangTuple) obj;
    }
}
