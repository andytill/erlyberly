package erlyberly;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.node.OtpUtil;

public class CrashReport {
    
    private static final OtpErlangAtom ATOM_INITIAL_CALL = OtpUtil.atom("initial_call");

    private static final OtpErlangAtom ATOM_PID = OtpUtil.atom("pid");

    private static final OtpErlangAtom ATOM_REGISTERED_NAME = OtpUtil.atom("registered_name");

    private static final OtpErlangAtom ATOM_ERROR_INFO = OtpUtil.atom("error_info");

    private static final OtpErlangAtom ATOM_FILE = OtpUtil.atom("file");
    
    private static final OtpErlangAtom ATOM_LINE = OtpUtil.atom("line");
    
    private final HashMap<Object, Object> crashProps;

    private final String registeredName;

    private final OtpErlangObject terms;

    private final OtpErlangTuple errorInfo;

    private final LocalDateTime date = LocalDateTime.now();
    
    public CrashReport(OtpErlangObject obj) {
        this.terms = obj;
        crashProps  = OtpUtil.propsToMap((OtpErlangList) obj);
        errorInfo = (OtpErlangTuple) crashProps.get(ATOM_ERROR_INFO);
        
        // pull out the register name of the crashing process
        Object aRegName = crashProps.get(ATOM_REGISTERED_NAME);
        if(OtpUtil.list().equals(aRegName)) {
            registeredName = "";
        }
        else {
            registeredName = aRegName.toString();
        }
    }
    
    <T> List<T> mapStackTraces(StackTraceFn<T> fn) {
        OtpErlangList stackTrace = (OtpErlangList) errorInfo.elementAt(2);
        ArrayList<T> result = new ArrayList<T>();
        
        for (OtpErlangObject obj : stackTrace) {
            OtpErlangTuple tuple = (OtpErlangTuple) obj;
            OtpErlangAtom module = (OtpErlangAtom) tuple.elementAt(0);
            OtpErlangAtom function = (OtpErlangAtom) tuple.elementAt(1);
            OtpErlangObject argsOrArity = tuple.elementAt(2);
            OtpErlangLong arity;
            if(argsOrArity instanceof OtpErlangLong) {
                arity = (OtpErlangLong) argsOrArity;
            }
            else {
                OtpErlangList list = (OtpErlangList) argsOrArity;
                arity = new OtpErlangLong(list.arity());
            }
            HashMap<Object, Object> fileLineProps = OtpUtil.propsToMap((OtpErlangList) tuple.elementAt(3));
            OtpErlangString file = (OtpErlangString) fileLineProps.get(ATOM_FILE);
            OtpErlangLong line = (OtpErlangLong) fileLineProps.get(ATOM_LINE);
            
            result.add(
                fn.invoke(module, function, arity, file.stringValue(), line)
            );
        }
        return result;
    }
    
    public interface StackTraceFn<T> {
        T invoke(OtpErlangAtom module, OtpErlangAtom function, OtpErlangLong arity, String file, OtpErlangLong line);
    }

    public OtpErlangPid getPid() {
        return (OtpErlangPid) crashProps.get(ATOM_PID);
    }

    public String getRegisteredName() {
        return registeredName;
    }

    public Object getProcessInitialCall() {
        return crashProps.get(ATOM_INITIAL_CALL);
    }

    public OtpErlangObject getProps() {
        return terms;
    }

    public OtpErlangAtom getErrorClass() {
        return (OtpErlangAtom) errorInfo.elementAt(0);
    }

    public OtpErlangObject getErrorReason() {
        return errorInfo.elementAt(1);
    }

    public LocalDateTime getDateTime() {
        return date;
    }

    public Optional<OtpErlangList> getCallArgs() {
        OtpErlangTuple staceTrace1 = (OtpErlangTuple) ((OtpErlangList) errorInfo.elementAt(2)).getHead();
        OtpErlangObject argsOrArity = staceTrace1.elementAt(2);
        if(argsOrArity instanceof OtpErlangList)
            return Optional.of((OtpErlangList)argsOrArity);
        else
            return Optional.empty();
    }
}
