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
package erlyberly.node;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.ericsson.otp.erlang.OtpConn;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangExternalFun;
import com.ericsson.otp.erlang.OtpErlangFun;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangMap;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;

/**
 * Sin bin for utils dealing with jinterface.
 */
public class OtpUtil {

    private static final OtpErlangAtom ERLYBERLY_RECORD_ATOM = OtpUtil.atom("erlyberly_record");

    private static final OtpErlangAtom ERLYBERLY_RECORD_FIELD_ATOM = OtpUtil.atom("erlyberly_record_field");

    public static final Set<Class<?>> CONTAINER_TERM_TYPES = new HashSet<Class<?>>(
        Arrays.asList(OtpErlangTuple.class, OtpErlangMap.class, OtpErlangList.class)
    );


    public static final Set<Class<?>> LARGE_TERM_TYPES = new HashSet<Class<?>>(
        Arrays.asList(OtpErlangFun.class, OtpErlangExternalFun.class)
    );

    private static final OtpErlangAtom TRUE_ATOM = new OtpErlangAtom(true);
    private static final OtpErlangAtom FALSE_ATOM = new OtpErlangAtom(false);
    private static final OtpErlangAtom CALL_ATOM = new OtpErlangAtom("call");
    private static final OtpErlangAtom USER_ATOM = new OtpErlangAtom("user");
    private static final OtpErlangAtom ERROR_ATOM = atom("error");
    public static final OtpErlangAtom OK_ATOM = atom("ok");


    public static OtpErlangTuple tuple(Object... elements) {
        OtpErlangObject[] tuple = toOtpElementArray(elements);

        return new OtpErlangTuple(tuple);
    }

    public static OtpErlangList list(Object... elements) {
        OtpErlangObject[] tuple = toOtpElementArray(elements);

        return new OtpErlangList(tuple);
    }

    private static OtpErlangObject[] toOtpElementArray(Object... elements) {
        OtpErlangObject[] tuple = new OtpErlangObject[elements.length];

        for(int i=0; i < elements.length; i++) {
            Object e = elements[i];
            if(e instanceof Integer) {
                tuple[i] = new OtpErlangLong((Integer)e);
            }
            else if(e instanceof OtpErlangObject) {
                tuple[i] = (OtpErlangObject) elements[i];
            }
            else if(e instanceof String) {
                tuple[i] = new OtpErlangString((String)elements[i]);
            }
            else if(e instanceof Boolean) {
                if(Boolean.TRUE.equals(e))
                    tuple[i] = TRUE_ATOM;
                else
                    tuple[i] = FALSE_ATOM;
            }
            else {
                throw new RuntimeException(e + " cannot be converted to an OtpErlangObject");
            }
        }
        return tuple;
    }


    public static OtpErlangAtom atom(String name) {
        return new OtpErlangAtom(name);
    }

    /**
     * Take an {@link OtpErlangList} of erlang key value tuples and converts it to a map.
     */
    public static Map<Object, Object> propsToMap(OtpErlangList pinfo) {
        HashMap<Object, Object> map = new HashMap<>();
        for (OtpErlangObject otpErlangObject : pinfo) {
            if(otpErlangObject instanceof OtpErlangTuple && ((OtpErlangTuple) otpErlangObject).arity() == 2) {
                OtpErlangTuple tuple = ((OtpErlangTuple) otpErlangObject);
                map.put(tuple.elementAt(0), tuple.elementAt(1));
            }
        }
        return map;
    }

    public static OtpErlangObject[] elementsForTerm(OtpErlangObject obj) {
        assert obj != null;

        if(obj instanceof OtpErlangTuple)
            return ((OtpErlangTuple) obj).elements();
        else if(obj instanceof OtpErlangList)
            return ((OtpErlangList) obj).elements();
        else
            throw new RuntimeException("No brackets for type " + obj.getClass());
    }



    public static boolean isTupleTagged(OtpErlangObject tag, OtpErlangObject result) {
        return isTupleTagged(tag, 0, result);
    }

    public static boolean isTupleTagged(OtpErlangObject tag, int index, OtpErlangObject result) {
        boolean r = false;

        if(result instanceof OtpErlangTuple) {
            OtpErlangTuple resultTuple = (OtpErlangTuple) result;
            r = resultTuple.arity() > 0 && resultTuple.elementAt(0).equals(tag);
        }

        return r;
    }

    /**
     * Checks if a term is tuple tagged with the erlyberly_record atom, meaning
     * it has metadata tagged with record and field names around the field values.
     */
    public static boolean isErlyberlyRecord(OtpErlangObject obj) {
        return isTupleTagged(ERLYBERLY_RECORD_ATOM, obj);
    }

    public static boolean isErlyberlyRecordField(OtpErlangObject obj) {
        return isTupleTagged(ERLYBERLY_RECORD_FIELD_ATOM, obj);
    }

    public static boolean isErrorReason(OtpErlangObject reason, OtpErlangObject error) {
        assert isTupleTagged(ERROR_ATOM, error) : "tuple " + error + "is not tagged with 'error'";
        return isTupleTagged(reason, 1, error);
    }

    public static OtpErlangList toOtpList(OtpErlangObject obj) {
        if(obj instanceof OtpErlangList) {
            return (OtpErlangList) obj;
        }
        else if(obj instanceof OtpErlangString) {
            OtpErlangString s = (OtpErlangString) obj;

            return new OtpErlangList(s.stringValue());
        }
        else {
            throw new ClassCastException("" + obj + " cannot be converted to an OtpErlangList");
        }
    }

    public static void sendRPC(OtpConn conn, OtpMbox m, OtpErlangAtom mod, OtpErlangAtom fun, OtpErlangList args) throws IOException {
        OtpErlangTuple rpcMessage = tuple(m.self(), tuple(CALL_ATOM, mod, fun, args, USER_ATOM));

        conn.send(m.self(), "rex", rpcMessage);
    }

    public static OtpErlangTuple receiveRPC(OtpMbox mbox) throws OtpErlangExit, OtpErlangDecodeException  {
        return receiveRPC(mbox,5000);
    }

    public static OtpErlangTuple receiveRPC(OtpMbox mbox ,long timeout) throws OtpErlangExit, OtpErlangDecodeException {
        return (OtpErlangTuple) mbox.receive(timeout);
    }

    public static OtpErlangObject tupleElement(int i, OtpErlangObject obj) {
        return ((OtpErlangTuple)obj).elementAt(i);
    }

    public static OtpErlangObject[] iterableElements(OtpErlangObject obj) {
        if(obj instanceof OtpErlangTuple)
            return ((OtpErlangTuple) obj).elements();
        else if(obj instanceof OtpErlangList)
            return ((OtpErlangList) obj).elements();
        else if(obj instanceof OtpErlangString) {
            OtpErlangString s = (OtpErlangString) obj;
            return new OtpErlangList(s.stringValue()).elements();
        }
        else {
            throw new RuntimeException("" + obj + " cannot return OtpErlangObject[]");
        }
    }

    /**
     * A short term can be displayed on a single line and does not have to be
     * broken down further.
     */
    public static boolean isLittleTerm(OtpErlangObject obj) {
        OtpErlangObject[] elements;

        if(obj instanceof OtpErlangList) {
            // if the list is empty consider it a little term
            return ((OtpErlangList)obj).arity() == 0;
        }
        else if(LARGE_TERM_TYPES.contains(obj.getClass())) {
            return false;
        }
        else if(CONTAINER_TERM_TYPES.contains(obj.getClass())) {
            elements = iterableElements(obj);
            // short lists and tuples which do not contain other short lists or
            // tuples are ok
            if(elements.length > 3)
                return false;
            }
        else {
            return true;
        }

        for (OtpErlangObject e : elements) {
            if(LARGE_TERM_TYPES.contains(e.getClass()) || CONTAINER_TERM_TYPES.contains(e.getClass())) {
                return false;
            }
            else if(e instanceof OtpErlangString) {
                int stringLength = ((OtpErlangString) e).stringValue().length();
                if(stringLength > 50)
                    return true;
            }
            else if(e instanceof OtpErlangBinary) {
                int binaryLength = ((OtpErlangBinary) e).size();
                if(binaryLength > 50)
                    return true;
            }

        }
        return true;
    }

    /**
     * jinterface interprets lists of integers OtpErlangString whatever
     * might be the intent, for example a list of function arguments `[10]`.
     *
     * This can cause ClassCastExceptions when something that is normally an
     * OtpErlangList comes back as an OtpErlangString, which does not inherit
     * from OtpErlangList!
     */
    public static OtpErlangList toErlangList(OtpErlangObject obj) {
        if(obj instanceof OtpErlangString) {
            return new OtpErlangList(((OtpErlangString)obj).stringValue());
        }
        else {
            // we have done our best to convert the nobbly string objects to
            // lists, if it fails just throw a ClassCastException
            return (OtpErlangList)obj;
        }
    }

    public static OtpErlangTuple toTuple(OtpErlangObject obj) {
        if(!(obj instanceof OtpErlangTuple)) {
            throw new ClassCastException(obj + " cannot be case to OtpErlangTuple.");
        }
        return (OtpErlangTuple) obj;
    }
}
