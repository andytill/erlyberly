package erlyberly.node;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;

import com.ericsson.otp.erlang.OtpConn;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangMap;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;

/**
 * Sin bin for utils dealing with jinterface.
 */
public class OtpUtil {

	private static final OtpErlangAtom TRUE_ATOM = new OtpErlangAtom(true);
	private static final OtpErlangAtom FALSE_ATOM = new OtpErlangAtom(false);
	private static final OtpErlangAtom call = new OtpErlangAtom("call");
	private static final OtpErlangAtom user = new OtpErlangAtom("user");
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
	public static HashMap<Object, Object> propsToMap(OtpErlangList pinfo) {
		HashMap<Object, Object> map = new HashMap<>();
		for (OtpErlangObject otpErlangObject : pinfo) {
			if(otpErlangObject instanceof OtpErlangTuple && ((OtpErlangTuple) otpErlangObject).arity() == 2) {
				OtpErlangTuple tuple = ((OtpErlangTuple) otpErlangObject);
				map.put(tuple.elementAt(0), tuple.elementAt(1));
			}
		}
		return map;
	}
	
	public static void otpObjectToString(OtpErlangObject obj, StringBuilder sb) {
		if(obj instanceof OtpErlangBinary) {
			sb.append(binaryToString((OtpErlangBinary) obj));
		}
		else if(obj instanceof OtpErlangPid) {
			sb.append(pidToString((OtpErlangPid) obj));
		}
		else if(obj instanceof OtpErlangTuple || obj instanceof OtpErlangList) {
			String brackets = bracketsForTerm(obj);
			OtpErlangObject[] elements = elementsForTerm(obj);
			
			sb.append(brackets.charAt(0));
			
			for(int i=0; i < elements.length; i++) {
				if(i != 0) {
					sb.append(", ");
				}
				otpObjectToString(elements[i], sb);
			}
			sb.append(brackets.charAt(1));
		}
		else if(obj instanceof OtpErlangString) {
		    sb.append(obj.toString().replace("\n", "\\n"));
		}
		else {
			sb.append(obj.toString());
		}
	}
	
	public static String bracketsForTerm(OtpErlangObject obj) {
		assert obj != null;
		
		if(obj instanceof OtpErlangTuple)
			return "{}";
		else if(obj instanceof OtpErlangList)
			return "[]";
		else
			throw new RuntimeException("No brackets for type " + obj.getClass());
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
	
	public static String pidToString(OtpErlangPid pid) {
		return "<0." + pid.id() + "." + pid.serial() + ">";
	}
	
	public static String binaryToString(OtpErlangBinary bin) {
		StringBuilder s = new StringBuilder("<<");
		
		boolean inString = false;
		
		for (int b : bin.binaryValue()) {
			if(b > 31 && b < 127) {
				if(!inString) {
					if(s.length() > 2) {
						s.append(", ");
					}
					
					s.append("\"");
				}
				inString = true;
				s.append((char)b);
			}
			else {
				if(inString) {
					s.append("\"");
					inString = false;
				}
				
				if(s.length() > 2) {
					s.append(", ");
				}

				if(b < 0) {
					b = 256 + b;
				}
				s.append(Integer.toString(b));
			}
		}
		
		if(inString) {
			s.append("\"");
		}
		
		s.append(">>");
		
		return s.toString();
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
    	OtpErlangTuple rpcMessage = tuple(m.self(), tuple(call, mod, fun, args, user));
    	
    	conn.send(m.self(), "rex", rpcMessage);
    }
    
    public static OtpErlangObject receiveRPC(OtpMbox mbox) throws OtpErlangExit, OtpErlangDecodeException {
    	OtpErlangTuple receive = (OtpErlangTuple) mbox.receive(5000);
    	
    	// FIXME handle timeouts
    	if(receive == null)
    		return null;
    	if(!isTupleTagged(atom("rex"), receive))
    		throw new RuntimeException("Expected tuple tagged with atom rex but got " + receive);
    	
    	return receive.elementAt(1);
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
		else
			throw new RuntimeException("" + obj + " cannot return OtpErlangObject[]");
			
	}
	
	public static HashSet<Class<?>> LARGE_TERM_TYPES = new HashSet<Class<?>>(
        Arrays.asList(OtpErlangList.class, OtpErlangTuple.class, OtpErlangMap.class)
    );
	
    /**
     * A short term can be displayed on a single line and does not have to be
     * broken down further.
     */
	public static boolean isLittleTerm(OtpErlangObject obj) {
        OtpErlangObject[] elements;
        
	    if(LARGE_TERM_TYPES.contains(obj.getClass())) {
            elements = iterableElements(obj);
	    }
	    else {
	        return true;
	    }
	    
        // short lists and tuples which do not contain other short lists or
        // tuples are ok
	    if(elements.length > 3)
	        return false;
	    
	    for (OtpErlangObject e : elements) {
            if(LARGE_TERM_TYPES.contains(e.getClass())) {
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
}
