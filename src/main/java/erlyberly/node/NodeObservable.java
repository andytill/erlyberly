package erlyberly.node;

import com.ericsson.otp.erlang.OtpErlangList;

import erlyberly.ErlyBerly;
import rx.Observable;
import rx.schedulers.Schedulers;

/**
 * Created by andytill on 23/11/2016.
 */
public class NodeObservable  {

    static Observable<?> rpc(String mod, String function, OtpErlangList args) {
        assert mod != null;
        assert function != null;
        assert args != null;
        return Observable.defer(() ->
                Observable.just(ErlyBerly.nodeAPI().rpc(mod, function, args))
        ).observeOn(Schedulers.io());
    }

/*    private OtpErlangObject rpc(OtpErlangAtom module, OtpErlangAtom function, OtpErlangList args) {
        final OtpMbox mailbox = self.createMbox();
        try {
            OtpUtil.sendRPC(connection, mailbox, module, function, args);
            return receiveRPC(5000, mailbox);
        }
        catch (Exception e) {
            throw new RuntimeException(e);
        }
        finally {
            mailbox.close();
        }
    }*/
}
