package org.yahor.imageprocessing;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

import java.awt.*;

public class Main extends Application {

    @Override
    public void start(Stage primaryStage) throws Exception{
        Parent root = FXMLLoader.load(getClass().getResource("main_stage.fxml"));
        primaryStage.setTitle("Image processor");
        Dimension screensSize = Toolkit.getDefaultToolkit().getScreenSize();
        primaryStage.setScene(new Scene(root, screensSize.getWidth() - 0.1 * screensSize.getWidth(),
                screensSize.getHeight()));
        primaryStage.show();
    }

    /*
    Variant 5. Previtt
     */
    public static void main(String[] args) {
        launch(args);
    }
}
