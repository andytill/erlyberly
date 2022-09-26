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

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangString;
import javafx.beans.property.LongProperty;
import javafx.beans.property.SimpleLongProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;

import java.util.Map;
import java.util.Objects;

/**
 * Domain object for an erlang process.
 */
public class ProcInfo implements Comparable<ProcInfo> {

    private static final OtpErlangAtom TOTAL_HEAP_SIZE_ATOM = new OtpErlangAtom("total_heap_size");

    private static final OtpErlangAtom STACK_SIZE_ATOM = new OtpErlangAtom("stack_size");

    private static final OtpErlangAtom HEAP_SIZE_ATOM = new OtpErlangAtom("heap_size");

    private static final OtpErlangAtom MSG_QUEUE_LEN_ATOM = new OtpErlangAtom("message_queue_len");

    private static final OtpErlangList EMPTY_LIST = new OtpErlangList();

    private static final OtpErlangAtom REGISTERED_NAME_ATOM = new OtpErlangAtom("registered_name");

    private static final OtpErlangAtom GLOBAL_NAME_ATOM = new OtpErlangAtom("global_name");

    private static final OtpErlangAtom PID_ATOM = new OtpErlangAtom("pid");

    private static final OtpErlangAtom REDUCTIONS_ATOM = new OtpErlangAtom("reductions");

    private StringProperty pid;

    private StringProperty processName;

    private LongProperty reductions;

    private LongProperty msgQueueLen;

    private LongProperty heapSize;

    private LongProperty stackSize;

    private LongProperty totalHeapSize;

    public void setTotalHeapSize(final long value) {
        this.totalHeapSizeProperty().set(value);
    }

    public long getTotalHeapSize() {
        return this.totalHeapSizeProperty().get();
    }

    public LongProperty totalHeapSizeProperty() {
        if (null == this.totalHeapSize) this.totalHeapSize = new SimpleLongProperty(this, "totalHeapSize");
        return this.totalHeapSize;
    }

    public void setStackSize(final long value) {
        this.stackSizeProperty().set(value);
    }

    public long getStackSize() {
        return this.stackSizeProperty().get();
    }

    public LongProperty stackSizeProperty() {
        if (null == this.stackSize) this.stackSize = new SimpleLongProperty(this, "stackSize");
        return this.stackSize;
    }

    public void setHeapSize(final long value) {
        this.heapSizeProperty().set(value);
    }

    public long getHeapSize() {
        return this.heapSizeProperty().get();
    }

    public LongProperty heapSizeProperty() {
        if (null == this.heapSize) this.heapSize = new SimpleLongProperty(this, "msgQueueLen");
        return this.heapSize;
    }

    public void setMsgQueueLen(final long value) {
        this.msgQueueLenProperty().set(value);
    }

    public long getMsgQueueLen() {
        return this.msgQueueLenProperty().get();
    }

    public LongProperty msgQueueLenProperty() {
        if (null == this.msgQueueLen) this.msgQueueLen = new SimpleLongProperty(this, "msgQueueLen");
        return this.msgQueueLen;
    }

    public void setPid(final String value) {
        this.pidProperty().set(value);
    }

    public String getPid() {
        return this.pidProperty().get();
    }

    public StringProperty pidProperty() {
        if (null == this.pid) this.pid = new SimpleStringProperty(this, "pid");
        return this.pid;
    }

    public void setProcessName(final String value) {
        this.processNameProperty().set(value);
    }

    public String getProcessName() {
        return this.processNameProperty().get();
    }

    public StringProperty processNameProperty() {
        if (null == this.processName) this.processName = new SimpleStringProperty(this, "processName");
        return this.processName;
    }

    public void setReductions(final long value) {
        this.reductionsProperty().set(value);
    }

    public long getReductions() {
        return this.reductionsProperty().get();
    }

    public LongProperty reductionsProperty() {
        if (null == this.reductions) this.reductions = new SimpleLongProperty(this, "reductions");
        return this.reductions;
    }

    public static ProcInfo toProcessInfo(final Map<Object, Object> propList) {
        Object processName = propList.get(REGISTERED_NAME_ATOM);
        final Object pid = ((OtpErlangString) propList.get(PID_ATOM)).stringValue();

        if (EMPTY_LIST.equals(processName)) {
            processName = "";
        }

        final ProcInfo processInfo;

        processInfo = new ProcInfo();
        final String localName = Objects.toString(processName, "");
        final String globalName = Objects.toString(propList.get(GLOBAL_NAME_ATOM), "");
        processInfo.setProcessName(localName + (null != localName && localName.isEmpty() ? "" : " ") + (null != globalName && globalName.isEmpty() ? "" : "(" + globalName + ")"));
        processInfo.setPid(Objects.toString(pid, ""));
        processInfo.setReductions(toLong(propList.get(REDUCTIONS_ATOM)));
        processInfo.setMsgQueueLen(toLong(propList.get(MSG_QUEUE_LEN_ATOM)));
        processInfo.setHeapSize(toLong(propList.get(HEAP_SIZE_ATOM)));
        processInfo.setStackSize(toLong(propList.get(STACK_SIZE_ATOM)));
        processInfo.setTotalHeapSize(toLong(propList.get(TOTAL_HEAP_SIZE_ATOM)));

        return processInfo;
    }

    private static long toLong(final Object object) {
        if (object instanceof OtpErlangLong) {
            return ((OtpErlangLong) object).longValue();
        }
        return 0;
    }

    /**
     * We shouldn't need to implement this but sometimes {@link TableView}
     * attempts to sort the  objects itself and tries to cast
     * to {@link Comparable}.
     * <p>
     * To avoid this exception we're just implementing comparable even if it
     * gives the wrong sort to what the user expected.
     */
    @Override
    public int compareTo(final ProcInfo o) {
        return this.getProcessName().compareTo(o.getProcessName());
    }

    public String getShortName() {
        final String processName2 = this.getProcessName();
        if (null != processName2 && !"".equals(this.getProcessName())) return this.getProcessName();
        return this.getPid();
    }
}
