package erlyberly.node;

import com.ericsson.otp.erlang.OtpErlangAtom;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class RecordManager {
    private final ConcurrentHashMap<RecordKey, List<String>> records = new ConcurrentHashMap<>();

    public List<String> get(final Object key) {
        return this.records.get(key);
    }

    public List<String> put(final RecordKey key, final List<String> value) {
        return this.records.put(key, value);
    }

    public static class RecordKey {
        private final OtpErlangAtom module, recordName;

        public RecordKey(final OtpErlangAtom module, final OtpErlangAtom recordName) {
            super();
            this.module = module;
            this.recordName = recordName;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((null == this.module) ? 0 : this.module.hashCode());
            result = prime * result + ((null == this.recordName) ? 0 : this.recordName.hashCode());
            return result;
        }

        @Override
        public boolean equals(final Object obj) {
            if (this == obj) return true;
            if (null == obj) return false;
            if (this.getClass() != obj.getClass()) return false;
            final RecordKey other = (RecordKey) obj;
            if (null == this.module) {
                if (null != other.module) return false;
            } else if (!this.module.equals(other.module)) return false;
            if (null == this.recordName) {
                return null == other.recordName;
            } else return this.recordName.equals(other.recordName);
        }
    }

    boolean isModuleManaged(final OtpErlangAtom moduleName) {
        for (final Map.Entry<RecordKey, List<String>> entry : this.records.entrySet()) {
            if (moduleName.equals(entry.getKey().module)) return true;
        }
        return false;
    }
}
