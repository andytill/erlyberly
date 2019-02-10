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

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.StackTraceView.ErlyberlyStackTraceElement;
import erlyberly.node.OtpUtil;
import javafx.application.Platform;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class StackTraceView extends ListView<ErlyberlyStackTraceElement> {
    @SuppressWarnings({ "unchecked", "rawtypes" })
    public StackTraceView() {
        applyStackTraceCellFactory((ListView)this);
    }

    private void applyStackTraceCellFactory(ListView<ErlyberlyStackTraceElement> listview) {
        listview.setCellFactory(new Callback<ListView<ErlyberlyStackTraceElement>, ListCell<ErlyberlyStackTraceElement>>() {
            @Override
            public ListCell<ErlyberlyStackTraceElement> call(ListView<ErlyberlyStackTraceElement> param) {
                return new ListCell<ErlyberlyStackTraceElement>() {
                    private final Hyperlink functionLink = new Hyperlink();
                    {
                        setGraphic(functionLink);
                        functionLink.setOnAction((e) -> {
                            ErlyberlyStackTraceElement stackElement = getItem();
                            if(stackElement == null)
                                return;
                            if(stackElement.isError())
                                return;
                            ModFunc mf = stackElement.getModFunc();
                            ErlyBerly.runIO(() -> {
                                try {
                                    String source = ErlyBerly.nodeAPI().moduleFunctionSourceCode(
                                            mf.getModuleName(), mf.getFuncName(), mf.getArity());
                                    Platform.runLater(() -> {
                                        ErlyBerly.showPane(
                                            "Crash Report Stack",
                                            ErlyBerly.wrapInPane(new CodeView(source))
                                        );
                                    });
                                } catch (Exception e1) {
                                    e1.printStackTrace();
                                }
                            });
                        });
                    }

                    @Override
                    public void updateItem(ErlyberlyStackTraceElement item, boolean empty) {
                        super.updateItem(item, empty);
                        if(item == null)
                            functionLink.setText("");
                        else
                            functionLink.setText(item.toString());
                    }
                };
            }
        });
    }

    public void populateFromCrashReport(CrashReport crashReport) {
        try {
            getItems().addAll(crashReport.<ErlyberlyStackTraceElement>mapStackTraces((module, function, arity, file, line) -> {
                try {
                    ModFunc modFunc = mfaToModFunc(module, function, arity);
                    return new ErlyberlyStackTraceElement(modFunc, file.toString(), line.longValue());
                }
                catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private ModFunc mfaToModFunc(OtpErlangAtom module, OtpErlangAtom function, OtpErlangLong arity) {
        boolean exported = false;
        boolean synthetic = false;
        try {
            return new ModFunc(module.toString(), function.toString(), arity.intValue(), exported, synthetic);
        }
        catch (OtpErlangRangeException e) {
            throw new RuntimeException(e);
        }
    }

    public void populateFromMfaList(OtpErlangList stackTrace) {
        if(stackTrace == null)
            return;
        try {
            for (OtpErlangObject obj : stackTrace) {
                if(obj instanceof OtpErlangString) {
                    OtpErlangString errorMessage = (OtpErlangString) obj;
                    getItems().add(new ErlyberlyStackTraceElement(errorMessage.stringValue()));
                }
                else {
                    OtpErlangTuple tuple = OtpUtil.toTuple(obj);
                    OtpErlangAtom module = (OtpErlangAtom) tuple.elementAt(0);
                    OtpErlangAtom function = (OtpErlangAtom) tuple.elementAt(1);
                    OtpErlangLong arity = (OtpErlangLong) tuple.elementAt(2);
                    ModFunc modFunc = mfaToModFunc(module, function, arity);
                    getItems().add(new ErlyberlyStackTraceElement(modFunc, "", 0L));
                }
            }
        }
        catch (Exception e) {
            // try/catch so if there is a problem decoding the process_dump, it won't
            // stop the trace log from being shown.
            e.printStackTrace();
        }
    }

    public boolean isStackTracesEmpty() {
        return getItems().isEmpty();
    }

    /**
     * Put erlyberly on the front of this because there is already a StaceTraceElement
     * class in the standard library.
     */
    static class ErlyberlyStackTraceElement {
        private final ModFunc modFunc;
        private final long line;
        private final String file;
        private final String error;

        public ErlyberlyStackTraceElement(ModFunc modFunc, String file, long line) {
            this.modFunc = modFunc;
            this.file = file;
            this.line = line;
            this.error = null;
        }

        public ErlyberlyStackTraceElement(String anError) {
            this.modFunc = null;
            this.file = null;
            this.line = -1L;
            this.error = anError;
        }

        public ModFunc getModFunc() {
            return modFunc;
        }

        public boolean isError() {
            return error != null;
        }

        @Override
        public String toString() {
            if(isError()) {
                return error;
            }
            String display = modFunc.toFullString();
            if(file != null && !file.isEmpty()) {
                display += "  (" + file + ":" + line +")";
            }
            return display;
        }
    }
}
