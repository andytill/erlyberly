package erlyberly.node;

import com.ericsson.otp.erlang.OtpErlangList;

import erlyberly.ErlyBerly;
import rx.Observable;
import rx.schedulers.Schedulers;

/**
 * Created by andytill on 23/11/2016.
 */
public class NodeObservable  {
    public static rx.Observable<?> rpc(String mod, String function, OtpErlangList args) {
        assert mod != null;
        assert function != null;
        assert args != null;
        return Observable.defer(() ->
                Observable.just(ErlyBerly.nodeAPI().rpc(mod, function, args))
        ).observeOn(Schedulers.io());
    }
}
