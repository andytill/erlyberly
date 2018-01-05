package erlyberly.node;

import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

import com.ericsson.otp.erlang.OtpErlangAtom;

public class RecordManager {
    private final ConcurrentHashMap<RecordKey, List<String>> records = new ConcurrentHashMap<>();

    public List<String> get(Object key) {
        return records.get(key);
    }

    public List<String> put(RecordKey key, List<String> value) {
        return records.put(key, value);
    }

    public static class RecordKey {
        private final OtpErlangAtom module, recordName;

        public RecordKey(OtpErlangAtom module, OtpErlangAtom recordName) {
            this.module = module;
            this.recordName = recordName;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((module == null) ? 0 : module.hashCode());
            result = prime * result + ((recordName == null) ? 0 : recordName.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            RecordKey other = (RecordKey) obj;
            if (module == null) {
                if (other.module != null)
                    return false;
            } else if (!module.equals(other.module))
                return false;
            if (recordName == null) {
                if (other.recordName != null)
                    return false;
            } else if (!recordName.equals(other.recordName))
                return false;
            return true;
        }
    }
}
