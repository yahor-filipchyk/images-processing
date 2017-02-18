package org.yahor.imageprocessing;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.stage.Stage;

import java.util.function.DoubleConsumer;
import java.util.function.IntConsumer;

public class InputParamsController {

    private DoubleConsumer handler;
    private int paramValue;

    @FXML protected TextField processingParam;
    @FXML protected Label paramLabel;

    @FXML
    public void doProcess(ActionEvent event) {
        String paramValue = processingParam.getText();
        if (paramValue != null && !paramValue.isEmpty()) {
            handler.accept(Double.parseDouble(paramValue));
        }
        ((Stage) processingParam.getScene().getWindow()).close();
    }

    protected void setImagesProcessorHandler(DoubleConsumer handler) {
        this.handler = handler;
    }
}
