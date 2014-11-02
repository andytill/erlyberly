package erlyberly.node;

import java.util.HashMap;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Sin bin for utils dealing with jinterface.
 */
public class OtpUtil {
	
	public static OtpErlangTuple tuple(OtpErlangObject... obj) {
		return new OtpErlangTuple(obj);
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
	
	public static String otpObjectToString(OtpErlangObject obj) {
		if(obj instanceof OtpErlangBinary)
			return binaryToString((OtpErlangBinary) obj);
		else if(obj instanceof OtpErlangPid) {
			return pidToString((OtpErlangPid) obj);
		}
		else
			return obj.toString();
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
}
