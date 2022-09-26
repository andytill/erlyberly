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
package erlyberly;

import com.ericsson.otp.erlang.*;
import erlyberly.node.OtpUtil;
import javafx.application.Platform;
import javafx.beans.property.*;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

public class TraceLog implements Comparable<TraceLog> {

    private static final OtpErlangAtom FN_ATOM = new OtpErlangAtom("fn");
    private static final OtpErlangAtom EXCEPTION_FROM_ATOM = new OtpErlangAtom("exception_from");
    private static final OtpErlangAtom RESULT_ATOM = new OtpErlangAtom("result");
    public static final OtpErlangAtom ATOM_PID = new OtpErlangAtom("pid");
    private static final OtpErlangAtom ATOM_REG_NAME = new OtpErlangAtom("reg_name");
    private static final OtpErlangAtom ATOM_UNDEFINED = new OtpErlangAtom("undefined");
    private static final Object TIMESTAMP_CALL_ATOM = new OtpErlangAtom("timetamp_call_us");
    private static final Object TIMESTAMP_RETURN_ATOM = new OtpErlangAtom("timetamp_return_us");

    private static final AtomicLong INSTANCE_COUNTER = new AtomicLong();

    private final Map<Object, Object> map;

    private final long instanceNum;

    private final SimpleStringProperty summary = new SimpleStringProperty("");

    private final SimpleLongProperty duration = new SimpleLongProperty(0L);

    private final SimpleStringProperty result = new SimpleStringProperty("");

    private final SimpleBooleanProperty complete = new SimpleBooleanProperty(false);

    private String tracePropsToString;

    private final String pid, registeredName, function, argsString;

    private final String cssClass;

    public TraceLog(final Map<Object, Object> map) {
        super();
        this.map = map;
        this.instanceNum = INSTANCE_COUNTER.incrementAndGet();
        this.pid = this.getPidString();
        this.registeredName = this.regNameString().intern();
        this.function = this.appendModFuncArity(new StringBuilder()).toString().intern();
        this.argsString = TraceLog.appendArgsToString(new StringBuilder(), this.getArgsList().elements()).toString();
        this.cssClass = null;
    }

    private TraceLog(final String aCssClass, final String text) {
        super();
        this.function = text;
        this.instanceNum = INSTANCE_COUNTER.incrementAndGet();
        this.map = new HashMap<>(0);
        this.cssClass = aCssClass;
        this.pid = "";
        this.registeredName = "";
        this.argsString = "";
    }

    public long getInstanceNum() {
        return this.instanceNum;
    }

    private String regNameString() {
        final Object object = this.map.get(ATOM_REG_NAME);
        if (ATOM_UNDEFINED.equals(object)) return "";
        return object.toString();
    }

    public SimpleStringProperty summaryProperty() {
        if (this.summary.get().isEmpty()) {
            this.summary.set(this.toString());
        }
        return this.summary;
    }

    @Override
    public String toString() {
        if (null == this.tracePropsToString) this.tracePropsToString = this.tracePropsToString();
        return this.tracePropsToString;
    }

    private String tracePropsToString() {
        final StringBuilder sb = new StringBuilder(1024);

        final boolean appendArity = false;
        this.toCallString(sb, appendArity);

        sb.append(" => ");

        if (this.map.containsKey(RESULT_ATOM)) {
            ErlyBerly.getTermFormatter().appendToString((OtpErlangObject) this.map.get(RESULT_ATOM), sb);
        } else if (this.isExceptionThrower()) {
            final OtpErlangTuple exception = (OtpErlangTuple) this.map.get(EXCEPTION_FROM_ATOM);
            ErlyBerly.getTermFormatter().exceptionToString((OtpErlangAtom) exception.elementAt(0), exception.elementAt(1));
        }
        return sb.toString();
    }

    private StringBuilder toCallString(final StringBuilder sb, final boolean appendArity) {
        final OtpErlangAtom regName = (OtpErlangAtom) this.map.get(ATOM_REG_NAME);

        if (null != regName && !ATOM_UNDEFINED.equals(regName)) {
            sb.append(regName.atomValue());
        } else {
            final OtpErlangString pidString = (OtpErlangString) this.map.get(ATOM_PID);
            if (null == pidString) sb.append("<NULL PID>");
            else sb.append(pidString.stringValue());
        }
        sb.append(" ");

        sb.append("+").append(this.durationFromMap()).append("us");

        this.appendModFuncArity(sb);

        return sb;
    }

    private long durationFromMap() {
        final Object tsCall = this.map.get(TIMESTAMP_CALL_ATOM);
        final Object tsReturn = this.map.get(TIMESTAMP_RETURN_ATOM);

        if (null == tsCall || null == tsReturn) return 0;

        return ((OtpErlangLong) tsReturn).longValue() - ((OtpErlangLong) tsCall).longValue();
    }

    /**
     * Returns an MFA.
     */
    private OtpErlangTuple getFunctionFromMap() {
        return (OtpErlangTuple) this.map.get(FN_ATOM);
    }

    boolean isExceptionThrower() {
        return this.map.containsKey(EXCEPTION_FROM_ATOM);
    }

    public StringBuilder appendFunctionToString(final StringBuilder sb) {
        final OtpErlangTuple tuple = this.getFunctionFromMap();

        return sb.append(ErlyBerly.getTermFormatter().modFuncArgsToString(tuple));
    }

    StringBuilder appendModFuncArity(final StringBuilder sb) {
        final OtpErlangTuple fn = this.getFunctionFromMap();
        // there might be no function if this TraceLog is acting as a NODE DOWN
        if (null == fn) {
            return sb;
        }
        return sb.append(ErlyBerly.getTermFormatter().modFuncArityToString(fn));
    }

    private static StringBuilder appendArgsToString(final StringBuilder sb, final OtpErlangObject[] elements) {
        if (0 < elements.length) ErlyBerly.getTermFormatter().appendToString(elements[0], sb);

        for (int i = 1; i < elements.length; i++) {
            sb.append(", ");
            ErlyBerly.getTermFormatter().appendToString(elements[i], sb);
        }
        return sb;
    }

    OtpErlangList getArgsList() {
        final OtpErlangTuple tuple = this.getFunctionFromMap();
        return OtpUtil.toOtpList(tuple.elementAt(2));
    }

    public String getPidString() {
        final OtpErlangString s = (OtpErlangString) this.map.get(ATOM_PID);
        if (null == s) return "";
        else return s.stringValue();
    }

    OtpErlangObject getResultFromMap() {
        final Object object = this.map.get(RESULT_ATOM);
        if (null == object) {
            final OtpErlangTuple exception = (OtpErlangTuple) this.map.get(EXCEPTION_FROM_ATOM);

            if (null != exception) {
                return exception.elementAt(1);
            }
        }
        return (OtpErlangObject) object;
    }

    @Override
    public int compareTo(final TraceLog o) {
        return Long.compare(this.instanceNum, o.instanceNum);
    }

    public void complete(final Map<Object, Object> resultMap) {
        this.tracePropsToString = null;
        final Object e = resultMap.get(EXCEPTION_FROM_ATOM);
        final Object ts = resultMap.get(TIMESTAMP_RETURN_ATOM);
        Object r = resultMap.get(RESULT_ATOM);
        if (null != e) {
            this.map.put(EXCEPTION_FROM_ATOM, e);
            if (null == r) {
                r = e;
            }
        }

        if (null != r) {
            this.map.put(RESULT_ATOM, r);
            final String resultString = ErlyBerly.getTermFormatter().toString((OtpErlangObject) r);
            this.result.set(resultString);
        }
        if (null != ts) this.map.put(TIMESTAMP_RETURN_ATOM, ts);

        this.duration.set(this.durationFromMap());

        Platform.runLater(() -> {
            this.summary.set(this.toString());
            this.complete.set(true);
        });
    }

    ReadOnlyBooleanProperty isCompleteProperty() {
        return this.complete;
    }

    boolean isComplete() {
        return !this.complete.get();
    }

    /**
     * A call string is the pid and function with arity.
     */
    String toCallString() {
        final StringBuilder sb = new StringBuilder(255);
        final boolean appendArity = true;
        return this.toCallString(sb, appendArity).toString();
    }

    public String getPid() {
        return this.pid;
    }

    public String getRegName() {
        return this.registeredName;
    }

    public long getDuration() {
        return this.duration.get();
    }

    public SimpleLongProperty durationProperty() {
        return this.duration;
    }

    public String getFunction() {
        return this.function;
    }

    public String getArgs() {
        return this.argsString;
    }

    public String getResult() {
        return this.result.get();
    }

    public SimpleStringProperty resultProperty() {
        return this.result;
    }

    String getCssClass() {
        return this.cssClass;
    }

    static TraceLog newBreakLog() {
        return new TraceLog("breaker-row", "BREAK");
    }

    static TraceLog newNodeDown() {
        return new TraceLog("breaker-row", "NODE DOWN");
    }

    static TraceLog newLoadShedding() {
        return new TraceLog("breaker-row", "LOAD SHEDDING");
    }

    OtpErlangList getStackTrace() {
        OtpErlangList stacktrace = (OtpErlangList) this.map.get(OtpUtil.atom("stack_trace"));
        if (null == stacktrace) stacktrace = new OtpErlangList();
        return stacktrace;
    }

    ModFunc getModFunc() {
        final OtpErlangTuple mfa = this.getFunctionFromMap();
        final OtpErlangAtom module = (OtpErlangAtom) mfa.elementAt(0);
        final OtpErlangAtom function = (OtpErlangAtom) mfa.elementAt(1);
        final int arity = ((OtpErlangList) mfa.elementAt(2)).arity();
        return new ModFunc(module.atomValue(), function.atomValue(), arity, false, false);
    }
}
