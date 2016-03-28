package erlyberly;

import java.util.HashMap;
import java.util.concurrent.atomic.AtomicLong;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.node.OtpUtil;
import javafx.application.Platform;
import javafx.beans.property.ReadOnlyBooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleStringProperty;

public class TraceLog implements Comparable<TraceLog> {

    public static final OtpErlangAtom EXCEPTION_FROM_ATOM = new OtpErlangAtom("exception_from");
    public static final OtpErlangAtom RESULT_ATOM = new OtpErlangAtom("result");
    public static final OtpErlangAtom ATOM_PID = new OtpErlangAtom("pid");
    public static final OtpErlangAtom ATOM_REG_NAME = new OtpErlangAtom("reg_name");
    public static final OtpErlangAtom ATOM_UNDEFINED = new OtpErlangAtom("undefined");
    private static final Object TIMESTAMP_CALL_ATOM = new OtpErlangAtom("timetamp_call_us");
    private static final Object TIMESTAMP_RETURN_ATOM = new OtpErlangAtom("timetamp_return_us");

    private static final AtomicLong INSTANCE_COUNTER = new AtomicLong();

    private final HashMap<Object, Object> map;

    private final long instanceNum;

    private final SimpleStringProperty summary = new SimpleStringProperty("");

    private final SimpleStringProperty duration = new SimpleStringProperty("");

    private final SimpleStringProperty result = new SimpleStringProperty("");

    private final SimpleBooleanProperty complete = new SimpleBooleanProperty(false);

    private String tracePropsToString;

    private final String pid, registeredName, function, argsString;

    public TraceLog(HashMap<Object, Object> map) {
        this.map = map;
        instanceNum = INSTANCE_COUNTER.incrementAndGet();
        pid = getPidString();
        registeredName = regNameString().intern();
        function =  appendModFuncArity(new StringBuilder()).toString().intern();
        argsString = appendArgsToString(new StringBuilder(), getArgsList().elements()).toString();
    }

    public long getInstanceNum() {
        return instanceNum;
    }

    private String regNameString() {
        Object object = map.get(ATOM_REG_NAME);
        if(ATOM_UNDEFINED.equals(object))
            return "";
        return object.toString();
    }

    public SimpleStringProperty summaryProperty() {
        if(summary.get().isEmpty()) {
            summary.set(toString());
        }
        return summary;
    }

    @Override
    public String toString() {
        if(tracePropsToString == null)
            tracePropsToString = tracePropsToString();
        return tracePropsToString;
    }

    private String tracePropsToString() {
        StringBuilder sb = new StringBuilder(1024);

        boolean appendArity = false;
        toCallString(sb, appendArity);

        sb.append(" => ");

        if(map.containsKey(RESULT_ATOM)) {
            ErlyBerly.getTermFormatter().appendToString((OtpErlangObject) map.get(RESULT_ATOM), sb);
        }
        else if(isExceptionThrower()) {
            OtpErlangTuple exception = (OtpErlangTuple) map.get(EXCEPTION_FROM_ATOM);
            ErlyBerly.getTermFormatter().exceptionToString(
                (OtpErlangAtom) exception.elementAt(0), exception.elementAt(1)
            );
        }
        return sb.toString();
    }

    private StringBuilder toCallString(StringBuilder sb, boolean appendArity) {
        OtpErlangAtom regName = (OtpErlangAtom) map.get(ATOM_REG_NAME);

        if(!ATOM_UNDEFINED.equals(regName)) {
            sb.append(regName.atomValue());
        }
        else {
            OtpErlangString pidString = (OtpErlangString) map.get(ATOM_PID);
            sb.append(pidString.stringValue());
        }
        sb.append(" ");

        appendTimeStampToString(sb);

        appendModFuncArity(sb);

        return sb;
    }

    private StringBuilder appendTimeStampToString(StringBuilder sb) {
        Object tsCall = map.get(TIMESTAMP_CALL_ATOM);
        Object tsReturn = map.get(TIMESTAMP_RETURN_ATOM);

        if(tsCall == null|| tsReturn == null)
            return sb;

        long us = ((OtpErlangLong)tsReturn).longValue() - ((OtpErlangLong)tsCall).longValue();

        sb.append("+")
          .append(us)
          .append("us ");
        return sb;
    }

    private OtpErlangTuple getFunctionFromMap() {
        return (OtpErlangTuple)map.get(new OtpErlangAtom("fn"));
    }

    public boolean isExceptionThrower() {
        return map.containsKey(EXCEPTION_FROM_ATOM);
    }

    public StringBuilder appendFunctionToString(StringBuilder sb) {
        OtpErlangTuple tuple = getFunctionFromMap();

        return sb.append(ErlyBerly.getTermFormatter().modFuncArgsToString(tuple));
    }

    public StringBuilder appendModFuncArity(StringBuilder sb) {
        return sb.append(ErlyBerly.getTermFormatter().modFuncArityToString(getFunctionFromMap()));
    }

    private StringBuilder appendArgsToString(StringBuilder sb, OtpErlangObject[] elements) {
        if(elements.length > 0)
            ErlyBerly.getTermFormatter().appendToString(elements[0], sb);

        for(int i=1; i<elements.length; i++) {
            sb.append(", ");
            ErlyBerly.getTermFormatter().appendToString(elements[i], sb);
        }
        return sb;
    }

    public OtpErlangList getArgsList() {
        OtpErlangTuple tuple = getFunctionFromMap();
        OtpErlangList args = OtpUtil.toOtpList(tuple.elementAt(2));
        return args;
    }

    public String getPidString() {
        OtpErlangString s = (OtpErlangString)map.get(ATOM_PID);

        return s.stringValue();
    }

    public OtpErlangObject getResultFromMap() {
        Object object = map.get(RESULT_ATOM);
        if(object == null) {
            OtpErlangTuple exception = (OtpErlangTuple) map.get(EXCEPTION_FROM_ATOM);

            if(exception != null) {
                return exception.elementAt(1);
            }
        }
        return (OtpErlangObject) object;
    }

    @Override
    public int compareTo(TraceLog o) {
        return Long.compare(instanceNum, o.instanceNum);
    }

    public void complete(HashMap<Object, Object> resultMap) {
        tracePropsToString = null;

        Object e = resultMap.get(EXCEPTION_FROM_ATOM);
        Object r = resultMap.get(RESULT_ATOM);
        Object ts = resultMap.get(TIMESTAMP_RETURN_ATOM);

        if(e != null)
            map.put(EXCEPTION_FROM_ATOM, e);
        if(r != null) {
            map.put(RESULT_ATOM, r);
            String resultString = ErlyBerly.getTermFormatter().toString((OtpErlangObject) r);
            result.set(resultString);
        }
        if(ts != null)
            map.put(TIMESTAMP_RETURN_ATOM, ts);

        duration.set(appendTimeStampToString(new StringBuilder()).toString());

        Platform.runLater(() -> { summary.set(toString()); complete.set(true); });
    }

    public ReadOnlyBooleanProperty isCompleteProperty() {
        return complete;
    }

    public boolean isComplete() {
        return complete.get();
    }

    /**
     * A call string is the pid and function with arity.
     */
    public String toCallString() {
        StringBuilder sb = new StringBuilder(255);
        boolean appendArity = true;
        return toCallString(sb, appendArity).toString();
    }

    public String getPid() {
        return pid;
    }

    public String getRegName() {
        return registeredName;
    }

    public String getDuration() {
        return duration.get();
    }

    public SimpleStringProperty durationProperty() {
        return duration;
    }

    public String getFunction() {
        return function;
    }

    public String getArgs() {
        return argsString;
    }

    public String getResult() {
        return result.get();
    }

    public SimpleStringProperty resultProperty() {
        return result;
    }
}
