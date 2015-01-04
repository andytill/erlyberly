package erlyberly.node;

import java.io.IOException;
import java.util.HashMap;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;

/**
 * Sin bin for utils dealing with jinterface.
 */
public class OtpUtil {

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
	
    public static void rpc(OtpErlangPid sendingPid, OtpErlangAtom mod, OtpErlangAtom fun, OtpErlangList args) throws IOException {
    	final OtpErlangObject[] rpc = new OtpErlangObject[2];
    	final OtpErlangObject[] call = new OtpErlangObject[5];

    	/* {self, { call, Mod, Fun, Args, user}} */

    	call[0] = new OtpErlangAtom("call");
    	call[1] = new OtpErlangAtom(mod);
    	call[2] = fun;
    	call[3] = args;
    	call[4] = new OtpErlangAtom("user");

    	rpc[0] = self.pid();
    	rpc[1] = new OtpErlangTuple(call);
    	new OtpMbox()
    	tuple(sendingPid, tuple());
    	OtpMbox m = null;
    	m.send(name, msg)
    	sendingPid.send("rex", new OtpErlangTuple(rpc));
        }
}
