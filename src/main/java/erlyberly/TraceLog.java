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
package erlyberly;

import static erlyberly.node.OtpUtil.atom;

import java.util.HashMap;
import java.util.concurrent.atomic.AtomicLong;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.format.TermFormatter;
import javafx.beans.property.ReadOnlyBooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleLongProperty;
import javafx.beans.property.SimpleStringProperty;

public class TraceLog implements Comparable<TraceLog> {

    private static final OtpErlangAtom EXCEPTION_FROM = atom("exception_from");
    public static final OtpErlangAtom TRACE_TS = atom("trace_ts");
    public static final OtpErlangAtom FN_ATOM = new OtpErlangAtom("fn");
    public static final OtpErlangAtom RESULT_ATOM = new OtpErlangAtom("result");
    public static final OtpErlangAtom PID_ATOM = new OtpErlangAtom("pid");
    public static final OtpErlangAtom ATOM_REG_NAME = new OtpErlangAtom("reg_name");
    public static final OtpErlangAtom UNDEFINED_ATOM = new OtpErlangAtom("undefined");

    private static final AtomicLong INSTANCE_COUNTER = new AtomicLong();

    private final long instanceNum;

    private final SimpleStringProperty summary = new SimpleStringProperty("");

    private final SimpleLongProperty duration = new SimpleLongProperty(0L);

    private final SimpleStringProperty result = new SimpleStringProperty("");

    private final SimpleBooleanProperty complete = new SimpleBooleanProperty(false);

    private final String pidString, argsString, functionString;

    private final OtpErlangPid pid;

    private final String cssClass;

    private String registeredName = "";

    private OtpErlangList stacktrace;

    private OtpErlangBinary stackTraceBinary;

    private boolean functionThrewException;

    private ModFunc modFunc;

    /**
     * The timestamp in microseconds of when the function was called.
     */
    private long callTime;

    private OtpErlangList args;

    private OtpErlangObject resultTerm;

    public TraceLog(OtpErlangTuple tuple, HashMap<OtpErlangPid, OtpErlangAtom> registeredProcesses, TermFormatter formatter) {
        instanceNum = INSTANCE_COUNTER.incrementAndGet();
        cssClass = null;
        // FIXME we don't know the registered name from the info we get from the file
        //       may have to get the registered names from tracing registrations
        //       http://erlang.org/doc/man/erlang.html#trace_3_trace_messages_register
        // {trace_ts,<0.79.0>,call,{code,all_loaded,[]},{1502,734505,108159}}
        OtpErlangObject messageType = tuple.elementAt(0);
        assert TRACE_TS.equals(messageType) : messageType;
        assert tuple.elementAt(1) instanceof OtpErlangPid : tuple.elementAt(1);
        pid = (OtpErlangPid) tuple.elementAt(1);
        OtpErlangAtom aRegName = registeredProcesses.get(pid);
        if(aRegName == null) {
            registeredName = "";
        }
        else {
            registeredName = aRegName.atomValue();
        }
        pidString = formatter.toString(tuple.elementAt(1));
        // we only accept trace types of `call` in the constructor
        OtpErlangObject traceType = tuple.elementAt(2);
        assert traceType instanceof OtpErlangAtom : traceType;
        assert traceType.equals(atom("call")) : traceType;
        OtpErlangObject mfaObject = tuple.elementAt(3);
        assert mfaObject instanceof OtpErlangTuple : mfaObject;
        OtpErlangTuple mfa = (OtpErlangTuple) mfaObject;
        assert mfa.arity() == 3 : mfa;
        assert mfa.elementAt(0) instanceof OtpErlangAtom : mfa;
        assert mfa.elementAt(1) instanceof OtpErlangAtom : mfa;
        assert mfa.elementAt(2) instanceof OtpErlangList : mfa;
        args = (OtpErlangList) mfa.elementAt(2);
        argsString = appendArgsToString(new StringBuilder(), getArgsList().elements()).toString();
        int arity = args.arity();
        modFunc = new ModFunc(
            ((OtpErlangAtom)mfa.elementAt(0)).atomValue(),
            ((OtpErlangAtom)mfa.elementAt(1)).atomValue(), arity, false, false);
        // stack trace as a binary
        assert tuple.elementAt(4) instanceof OtpErlangBinary : tuple.elementAt(4);
        stackTraceBinary = (OtpErlangBinary) tuple.elementAt(4);
        // the three element tuple timestamp
        int timestampIndex = tuple.arity()-1;
        assert tuple.elementAt(timestampIndex) instanceof OtpErlangTuple : "TRACE WAS " + tuple + " timestamp was " + formatter.toString(tuple.elementAt(timestampIndex));
        callTime = tupleToTimestamp((OtpErlangTuple) tuple.elementAt(timestampIndex));
        functionString = formatter.modFuncArityToString(
            modFunc.getModuleName(), modFunc.getFuncName(), modFunc.getArity());
    }

    /**
     * Convert the annoying erlang 3 element timestamp to microseconds.
     */
    private long tupleToTimestamp(OtpErlangTuple tuple) {
        assert tuple.arity() == 3;
        long mega, sec, micros;
        mega = tupleElementToLong(0, tuple);
        sec = tupleElementToLong(1, tuple);
        micros = tupleElementToLong(2, tuple);
        return (((mega * 1000000) + sec) * 1000000) + micros;
    }

    private long tupleElementToLong(int index, OtpErlangTuple tuple) {
        assert index < tuple.arity() : tuple;
        assert tuple.elementAt(index) instanceof OtpErlangLong : tuple;
        return ((OtpErlangLong)tuple.elementAt(index)).longValue();
    }

    public TraceLog(String aCssClass, String text) {
        instanceNum = INSTANCE_COUNTER.incrementAndGet();
        cssClass = aCssClass;
        pidString = "";
        registeredName = "";
        argsString = "";
        pid = null;
        // this is what shows up in the table
        functionString = text;
    }

    public long getInstanceNum() {
        return instanceNum;
    }

    public SimpleStringProperty summaryProperty() {
        if(summary.get().isEmpty()) {
            summary.set(toString());
        }
        return summary;
    }

/*    private String tracePropsToString() {
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
    }*/

    private StringBuilder toCallString(StringBuilder sb, boolean appendArity) {
        if(!"".equals(registeredName)) {
            sb.append(registeredName);
        }
        else {
            sb.append(pidString);
        }
        sb.append(" ");

        sb.append("+").append(duration).append("us");

        appendModFuncArity(sb);

        return sb;
    }

    public boolean isExceptionThrower() {
        return functionThrewException;
    }

    public StringBuilder appendModFuncArity(StringBuilder sb) {
        return sb.append(functionString);
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
        return args;
    }

    public OtpErlangObject getResultFromMap() {
        return resultTerm;
    }

    @Override
    public int compareTo(TraceLog o) {
        return Long.compare(instanceNum, o.instanceNum);
    }

    /**
     * Called when we get a return_from trace which completes t
     */
    public void complete(OtpErlangTuple tuple, TermFormatter formatter) {
        assert TRACE_TS.equals(tuple.elementAt(0));
        assert tuple.elementAt(1) instanceof OtpErlangPid : tuple;
        assert pid.equals(tuple.elementAt(1)) : tuple;
        assert tuple.elementAt(2) instanceof OtpErlangAtom : tuple;
        OtpErlangAtom traceType = (OtpErlangAtom) tuple.elementAt(2);
        assert atom("return_from").equals(traceType) || EXCEPTION_FROM.equals(traceType) : tuple;
        functionThrewException = EXCEPTION_FROM.equals(traceType);
        // index 3 is the mfa which we already have from the call trace message
        resultTerm = tuple.elementAt(4);
        result.set(formatter.toString(resultTerm));
        OtpErlangTuple tsTuple = (OtpErlangTuple) tuple.elementAt(5);
        long returnTs = tupleToTimestamp(tsTuple);
        duration.set(returnTs - callTime);
        complete.set(true);
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

    public OtpErlangPid getPid() {
        return pid;
    }

    public String getRegName() {
        return registeredName;
    }

    public long getDuration() {
        return duration.get();
    }

    public SimpleLongProperty durationProperty() {
        return duration;
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

    public String getCssClass() {
        return cssClass;
    }

    public static TraceLog newBreakLog() {
        return new TraceLog("breaker-row", "BREAK");
    }

    public static TraceLog newNodeDown() {
        return new TraceLog("breaker-row", "NODE DOWN");
    }

    public static TraceLog newLoadShedding() {
        return new TraceLog("breaker-row", "LOAD SHEDDING");
    }

    public OtpErlangList getStackTrace() {
        return stacktrace;
    }

    public void setStackTrace(OtpErlangList stacktrace) {
        this.stacktrace = stacktrace;
    }

    public ModFunc getModFunc() {
        return modFunc;
    }

    public String getPidString() {
        return pidString;
    }

    public String getFunction() {
        return functionString;
    }

    public OtpErlangBinary getStackTraceBinary() {
        return stackTraceBinary;
    }

    public void setStackTraceBinary(OtpErlangBinary stackTraceBinary) {
        this.stackTraceBinary = stackTraceBinary;
    }
}
