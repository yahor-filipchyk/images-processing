package org.yahor.imageprocessing;

import javafx.application.Platform;
import javafx.embed.swing.SwingFXUtils;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.chart.BarChart;
import javafx.scene.chart.CategoryAxis;
import javafx.scene.chart.NumberAxis;
import javafx.scene.chart.XYChart;
import javafx.scene.control.Label;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.StackPane;
import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;
import javafx.stage.Stage;
import org.yahor.imageprocessing.processors.*;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.concurrent.CompletableFuture;
import java.util.function.DoubleConsumer;
import java.util.function.Supplier;

public class Controller implements Initializable {

    private final FileChooser fileChooser = new FileChooser();
    private File currentFile;
    private BufferedImage currentImage;
    private BufferedImage processedImage;
    private Hopfield hopfield = new Hopfield();

    @FXML private GridPane root;
    @FXML private ImageView imageView;
    @FXML private StackPane imageViewWrapper;
    @FXML private ImageView processedImageView;
    @FXML private StackPane processedImageViewWrapper;
    @FXML private Label statusBar;

    @FXML
    public void openFile(ActionEvent event) {
        fileChooser.setTitle("Open image file");
        currentFile = fileChooser.showOpenDialog(root.getScene().getWindow());
        currentImage = null;
        if (currentFile != null) {
            setImage(imageView, currentFile);
            statusBar.setText(currentFile.getName() + " was opened");
            imageView.fitWidthProperty().bind(imageViewWrapper.widthProperty());
            imageView.fitHeightProperty().bind(imageViewWrapper.heightProperty());
            getImage();
        }
    }

    @FXML
    public void saveFile(ActionEvent event) throws IOException {
        fileChooser.setTitle("Save image file");
        File outputFIle = fileChooser.showSaveDialog(root.getScene().getWindow());
        if (outputFIle != null && processedImage != null) {
            if (!outputFIle.exists()) {
                outputFIle.createNewFile();
            }
            ImageIO.write(processedImage, "jpg", outputFIle);
            statusBar.setText(outputFIle.getName() + " was saved");
        }
    }

    @FXML
    public void exit(ActionEvent event) {
        Platform.exit();
    }

    @FXML
    public void increaseBrightness(ActionEvent event) throws IOException {
        setUpImageProcessingWithParameter("Input fmax parameter", "fmax", this::increaseBrightness);
    }

    @FXML
    public void decreaseBrightness(ActionEvent event) throws IOException {
        setUpImageProcessingWithParameter("Input gmax parameter", "gmax", this::decreaseBrightness);
    }

    @FXML
    public void toBlackWhite(ActionEvent event) throws IOException {
        setUpImageProcessingWithParameter("Input brightness threshold", "fmax", this::toBlackWhite);
    }

    @FXML
    public void classify(ActionEvent event) throws IOException {
        setUpImageProcessingWithParameter("Input number of classes", "Classes", this::classify);
    }

    @FXML
    public void filter(ActionEvent event) {
        applyProcessingAsync("Applying filter...",
                String.format("Filter for the image %s was applied", currentFile.getName()),
                () -> ImageProcessor.prewittFilter(getImage()));
    }

    @FXML
    public void gaussian(ActionEvent event) throws IOException {
        setUpImageProcessingWithParameter("Input sigma value", "sigma", this::gaussian);
    }

    @FXML
    public void lowBrightnessFilter(ActionEvent event) {
        applyProcessingAsync("Applying filter...",
                String.format("Filter for the image %s was applied", currentFile.getName()),
                () -> ImageProcessor.lowBrightnessFilter(getImage()));
    }

    @FXML
    public void diffOfGaussians(ActionEvent event) throws IOException {
        setUpImageProcessingWithParameter("Input sigma ratio value", "ratio", this::dog);
    }

    @FXML
    public void teachHopfield(ActionEvent event) {
        DirectoryChooser dirChooser = new DirectoryChooser();
        dirChooser.setInitialDirectory(new File(System.getProperty("user.home") + File.separator + "Documents"));
        File lettersDir = dirChooser.showDialog(null);
        if (lettersDir == null) {
            return;
        }
        File[] letters = lettersDir.listFiles();
        List<BufferedImage> images = new ArrayList<>(letters.length);
        for (File letter : letters) {
            try {
                BufferedImage image = ImageIO.read(letter);
                if (image != null) {
                    images.add(image);
                }
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }
        hopfield.teach(images);
    }

    @FXML
    public void applyNoise(ActionEvent event) throws IOException {
        setUpImageProcessingWithParameter("Input noise percentage", "%", this::applyNoise);
    }

    @FXML
    public void restoreImage(ActionEvent event) {
        applyProcessingAsync("Restoring an image...",
                String.format("The image %s is restored", currentFile.getName()),
                () -> hopfield.detect(getImage()));
    }

    @FXML
    public void showHistogram(ActionEvent event) {
        if (currentFile == null || !currentFile.exists()) {
            return;
        }
        Map<Integer, Integer> histogram = ImageProcessor.getHistogram(getImage());
        renderHistogram(histogram, currentFile.getName());
    }

    @FXML
    public void showHistogramForProcessed(ActionEvent event) {
        if (processedImage == null) {
            return;
        }
        Map<Integer, Integer> histogram = ImageProcessor.getHistogram(processedImage);
        renderHistogram(histogram, currentFile.getName() + " processed");
    }

    @FXML
    public void showAreas(ActionEvent event) {
        viewResult(ImageProcessor.showLabels(this.processedImage));
    }

    @FXML
    public void resetImage(ActionEvent event) {
        processedImage = currentImage;
        previewResult(processedImage);
    }

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        File fileChooserFolder = new File(System.getProperty("user.home") + File.separator + "Documents");
        System.out.println(fileChooserFolder);
        fileChooser.setInitialDirectory(fileChooserFolder);
        fileChooser.getExtensionFilters().addAll(
                new FileChooser.ExtensionFilter("All Images", "*.*"),
                new FileChooser.ExtensionFilter("JPG", "*.jpg"),
                new FileChooser.ExtensionFilter("PNG", "*.png"),
                new FileChooser.ExtensionFilter("JPEG", "*.jpeg")
        );
        File initPicture = new File(fileChooserFolder, "icon.png");
        if (initPicture.exists()) {
            setImage(imageView, initPicture);
            setImage(processedImageView, initPicture);
        }
    }

    private void previewResult(BufferedImage processedImage) {
        this.processedImage = processedImage;
        processedImageView.setImage(SwingFXUtils.toFXImage(processedImage, null));
        processedImageView.fitWidthProperty().bind(processedImageViewWrapper.widthProperty());
        processedImageView.fitHeightProperty().bind(processedImageViewWrapper.heightProperty());
    }

    private void viewResult(BufferedImage processedImage) {
//        this.processedImage = processedImage;
        processedImageView.setImage(SwingFXUtils.toFXImage(processedImage, null));
        processedImageView.fitWidthProperty().bind(processedImageViewWrapper.widthProperty());
        processedImageView.fitHeightProperty().bind(processedImageViewWrapper.heightProperty());
    }

    private void setUpImageProcessingWithParameter(String dialogTitle, String parameterName, DoubleConsumer processor)
            throws IOException {
        FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("params.fxml"));
        Parent root = fxmlLoader.load();
        Stage stage = new Stage();
        stage.setTitle(dialogTitle);
        Scene scene = new Scene(root, 280, 100);
        stage.setScene(scene);
        ((InputParamsController) fxmlLoader.getController()).paramLabel.setText(parameterName);
        ((InputParamsController) fxmlLoader.getController()).processingParam.requestFocus();
        ((InputParamsController) fxmlLoader.getController()).setImagesProcessorHandler(processor);
        stage.show();
    }

    private void applyProcessingAsync(String startingMessage, String finishingMessage,
                                      Supplier<BufferedImage> processingFunc) {
        statusBar.setText(startingMessage);
        CompletableFuture.supplyAsync(processingFunc).thenAccept(image ->
                        Platform.runLater(() -> {
                            previewResult(image);
                            statusBar.setText(finishingMessage);
                        })
        );
    }

    private void increaseBrightness(Double fmax) {
        applyProcessingAsync("Increasing brightness...",
                String.format("Brightness for the image %s was increased", currentFile.getName()),
                () -> ImageProcessor.prepareIncrease(getImage(), fmax.intValue()));
    }

    private void decreaseBrightness(Double gmax) {
        applyProcessingAsync("Decreasing brightness...",
                String.format("Brightness for the image %s was decreased", currentFile.getName()),
                () -> ImageProcessor.prepareDecrease(getImage(), gmax.intValue()));
    }

    private void toBlackWhite(Double fmax) {
        applyProcessingAsync("Converting to binary...",
                String.format("The image %s was converted to binary", currentFile.getName()),
                () -> ImageProcessor.toBlackWhite(getImage(), fmax.intValue()));
    }

    private void applyNoise(Double percentage) {
        applyProcessingAsync("Noising the image...",
                String.format("The image %s was noised", currentFile.getName()),
                () -> Hopfield.noise(getImage(), percentage.intValue()));
    }

    private void gaussian(Double sigma) {
        applyProcessingAsync("Applying Gaussian filter...",
                String.format("The image %s was blurred using Gaussian filter", currentFile.getName()),
                () -> ImageProcessor.gaussianFilter(getImage(), sigma.floatValue()));
    }

    private void dog(Double sigmaRatio) {
        applyProcessingAsync("Finding blobs...",
                String.format("Blobs on the image %s were found", currentFile.getName()),
                () -> BlobsDetector.showBlobs(getImage(), sigmaRatio.floatValue()));
    }


    private void classify(Double classes) {
        statusBar.setText("Clustering...");
        CompletableFuture.supplyAsync(() -> Clustering.classify(getImage(), classes.intValue())).thenAccept(image ->
                        Platform.runLater(() -> {
                            viewResult(image);
                            statusBar.setText(String.format("Objects on the image %s were classified", currentFile.getName()));
                        })
        );
//        applyProcessingAsync("Clustering...",
//                String.format("Objects on the image %s were classified", currentFile.getName()),
//                () -> ImageProcessor.classify(getImage(), classes));
    }

    private BufferedImage getImage() {
        if (currentImage == null) {
            try {
                currentImage = ImageIO.read(currentFile);
                processedImage = currentImage;
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }
        return processedImage;
    }

    private void renderHistogram(Map<Integer, Integer> histogram, String title) {
        Stage stage = new Stage();
        stage.setTitle(title);
        CategoryAxis xAxis = new CategoryAxis();
        NumberAxis yAxis = new NumberAxis();
        BarChart<String, Number> chart = new BarChart<String, Number>(xAxis, yAxis);
        chart.setTitle("Brightness histogram");
        xAxis.setLabel("Brightness");
        yAxis.setLabel("Points");
        XYChart.Series series = new XYChart.Series();
        for (int i = 0; i <= 255; i++) {
            if (histogram.containsKey(i)) {
                series.getData().add(new XYChart.Data(i + "", histogram.get(i)));
            } else {
                series.getData().add(new XYChart.Data(i + "", 0));
            }
        }
        Scene scene = new Scene(chart, 1000, 600);
        chart.getData().addAll(series);
        chart.setBarGap(0);
        chart.setCategoryGap(1);
        chart.setLegendVisible(false);
        stage.setScene(scene);
        stage.show();
    }

    private void setImage(ImageView imageView, File imageFile) {
        try {
            Image image = new Image(new FileInputStream(imageFile));
            imageView.setImage(image);
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
}
