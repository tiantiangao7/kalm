package main.java.edu.stonybrook.cs.ui;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javafx.application.Application;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.Event;
import javafx.event.EventHandler;
import javafx.geometry.Pos;
import javafx.geometry.Rectangle2D;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableColumn.CellDataFeatures;
import javafx.scene.control.TableView;
import javafx.scene.control.TextArea;
import javafx.scene.control.Tooltip;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import javafx.stage.Screen;
import javafx.stage.Stage;
import javafx.util.Callback;
import main.java.edu.stonybrook.cs.fpparser.FrameDescriptionPredicate;
import main.java.edu.stonybrook.cs.fpparser.SemanticLinkAddition;
import main.java.edu.stonybrook.cs.fpparser.SemanticLinkOverride;
import main.java.edu.stonybrook.cs.fpparser.SemanticScoreParameters;
import main.java.edu.stonybrook.cs.fpparser.SynsetOverride;
import main.java.edu.stonybrook.cs.frame.Frame;
import main.java.edu.stonybrook.cs.frame.FrameElement;
import main.java.edu.stonybrook.cs.frameextraction.FrameExtractor;
import main.java.edu.stonybrook.cs.query.QueryProcessing;
import main.java.edu.stonybrook.cs.util.PrologConnector;

public class KAM extends Application {
	private Text ulrResult = new Text("");
	public static String ontVal = "BabelNet";
	
	@Override
	public void start(Stage stage) {
		FrameDescriptionPredicate.Parse();
		SemanticLinkOverride.initialize();
		SemanticScoreParameters.initialize();
		SynsetOverride.initialize();
		SemanticLinkAddition.initialize();

		TextArea textArea = new TextArea();
		VBox vbox = new VBox();
		vbox.setAlignment(Pos.TOP_RIGHT);

		Rectangle2D primaryScreenBounds = Screen.getPrimary().getVisualBounds();
		
		HBox hbox = new HBox();
		hbox.setAlignment(Pos.TOP_RIGHT);
		Button button = new Button("Parse");
		button.setMinWidth(150);
		button.setMinHeight(50);
		
		Button batchButton = new Button("Batch");
		batchButton.setMinWidth(150);
		batchButton.setMinHeight(50);
		
		ComboBox<String> ontology = new ComboBox<String>();
		ontology.getItems().addAll("BabelNet",
				"MetaQA");
		ontology.setPromptText("BabelNet");
		ontology.valueProperty().addListener(new ChangeListener<String>() {
            @Override 
            public void changed(ObservableValue<? extends String> ov, String t, String t1) {                
                ontVal = t1;
                System.out.println(ontVal);
            }    
        });
		
		hbox.getChildren().addAll(ontology, button, batchButton);

		Text parsingResult = new Text("");
		parsingResult.setId("parsingresulttext");;
		VBox v2 = new VBox();
		v2.setAlignment(Pos.TOP_LEFT);
		v2.getChildren().add(parsingResult);
		
		ulrResult.setId("ulrresulttext");
		VBox v3 = new VBox();
		v3.setAlignment(Pos.TOP_LEFT);
		v3.getChildren().add(ulrResult);
		
		TabPane tabPane = new TabPane();
		
		ObservableList<String> sentenceData = FXCollections.observableArrayList();
		TableView<String> sentenceTable = new TableView<String>(sentenceData);
				
		initSentenceTable(sentenceTable, sentenceData, primaryScreenBounds.getWidth() - 1, tabPane);
		sentenceTable.setItems(sentenceData);

		sentenceTable.setMaxHeight(200);

		batchButton.setOnAction(action -> {
			batchProcessing();
		});
		
		button.setOnAction(action -> {
			String sentence = textArea.getText().trim();
			//textArea.setText("");
			tabPane.getTabs().clear();
			parsingResult.setText("");
			sentenceData.clear();
			ulrResult.setText("");
			
			if(sentence.length() != 0)
			{
				QueryProcessing.ClearVarWordIndexSet();
				if(QueryProcessing.IsQuery(sentence))
				{
					QueryProcessing.PreProcessQuery(sentence);
				}
				setSentenceParsingQuery(sentence.replace("$", ""));
				PrologConnector.ExecutePrologQuery();
				if(QueryProcessing.IsQuery(sentence))
				{
					QueryProcessing.ExtractImplicitVar();
				}
				String result = getParsingError();
				if(result != null)
				{
					parsingResult.setText("    " + result);
				}
				else
				{
					List<String> temp = sentenceToList(sentence);
					for(String s : temp)
					{
						sentenceData.add(s);
					}
					sentenceTable.refresh();
				}
			}
			else
			{
				parsingResult.setText("    Empty input!");
			}
			System.out.println("Done");
		});
		
		vbox.getChildren().addAll(textArea, hbox, v2, sentenceTable, tabPane, v3);
		Scene scene = new Scene(vbox, 1100, primaryScreenBounds.getHeight() - 200);
		scene.getStylesheets().add("main/java/edu/stonybrook/cs/ui/kamStyle.css");
		stage.setTitle("Knowledge Acquisition Logic Machine");
		stage.setScene(scene);
		stage.show();
	}

	private Tab ExistFrameTab(final TabPane tabPane, Frame frame) {
		for (int i = 0; i < tabPane.getTabs().size(); i++) {
			if (tabPane.getTabs().get(i).getText().equals(frame.getFrameName())) {
				return tabPane.getTabs().get(i);
			}
		}
		return null;

	}

	private void AddRowToTab(final Tab tab, Frame frame) {
		TableView<Frame> table = (TableView<Frame>) tab.getContent();
		ObservableList<Frame> data = table.getItems();
		data.add(frame);
	}

	private Tab addNewTab(final TabPane tabPane, Frame frame) {
		Tab newTab = new Tab(frame.getFrameName());
		newTab.setClosable(true);

		newTab.setOnClosed(new EventHandler<Event>() {
			@Override
			public void handle(Event event) {
				tabPane.getTabs().remove(newTab);
			}
		});

		ObservableList<Frame> data = FXCollections.observableArrayList();
		TableView<Frame> table = new TableView<Frame>(data);
		ArrayList<FrameElement> frameElementList = frame.getAllFrameElements();
		data.add(frame);
		
		for (int i = 0; i < frameElementList.size(); i++) {
			final int j = i;
			TableColumn<Frame, FrameElement> col = new TableColumn<Frame, FrameElement>();
			Label label = new Label(frameElementList.get(i).getFEName());
			String synsetGloss = frameElementList.get(i).getFENameBabelSynsetGloss();
			label.setTooltip(new Tooltip(synsetGloss));
			col.setGraphic(label);
			col.setCellValueFactory(param -> new ReadOnlyObjectWrapper<>(
					param.getValue().getAllFrameElements().get(j)));

			col.setCellFactory(
					new Callback<TableColumn<Frame, FrameElement>, TableCell<Frame, FrameElement>>() {
						@Override
						public TableCell<Frame, FrameElement> call(
								TableColumn<Frame, FrameElement> param) {
							TableCell<Frame, FrameElement> cell = new TableCell<Frame, FrameElement>() {								
								@Override
								public void updateItem(FrameElement item, boolean empty) {
									int rowIndex = this.getIndex();
									if (item != null) {
										VBox vb = new VBox();
										vb.setAlignment(Pos.CENTER);
										Label feVal = new Label(item.getFEVal());
										Label affinityScore = new Label();
										final ComboBox<String> synsetComboBox;										
										if (item.getFEVal() != null) {
											if(!item.getFEValQuantity().equals("1")&&!item.getFEValQuantity().equals("unknown"))
											{
												feVal.setText(item.getFEValQuantity() + " " + item.getFEVal());
											}											
											synsetComboBox = new ComboBox<>();
											synsetComboBox.getStyleClass().add("combo-box");
											synsetComboBox.getStyleClass().add("combo-box-popup");
											synsetComboBox.getStyleClass().add("arrow");
											synsetComboBox.getStyleClass().add("arrow-button");
											synsetComboBox.setPrefWidth(150);

											synsetComboBox
													.setCellFactory(new Callback<ListView<String>, ListCell<String>>() {
														@Override
														public ListCell<String> call(ListView<String> l) {
															return new ListCell<String>() {
																@Override
																protected void updateItem(String synsetID, boolean empty) {
																	super.updateItem(synsetID, empty);														
																	if (synsetID == null || empty || synsetID.length() == 0) {
																		setGraphic(null);
																	} else {
																		setText(synsetID);
																		String babelSynsetGloss = 
																				item.getFEValBabelSynsetGloss(synsetID);
																		Tooltip tooltip = new Tooltip(babelSynsetGloss);
																		setTooltip(tooltip);
																		setAlignment(Pos.CENTER);
																	}
																}
															};
														}
													});

											synsetComboBox.getSelectionModel().selectedItemProperty()
													.addListener(new ChangeListener<String>() {
														public void changed(
																ObservableValue<? extends String> observable,
																String oldSynsetID, String newSynsetID) {
															Double tempScore = item.getFEAffinityScore(newSynsetID);
															item.changeFEValBabelSynsetID(newSynsetID);
															Frame tempFrame = data.get(rowIndex);
															tempFrame.resetFrameScore();
															System.out.println(tempFrame.getFrameScoreStr());
															affinityScore.setText(String.format("%.3f",tempScore));
															System.out.println("Value is: " + newSynsetID);
															table.refresh();
															String babelSynsetGloss = item.getFEValBabelSynsetGloss(newSynsetID);
															Tooltip tooltip = new Tooltip(babelSynsetGloss);
															setTooltip(tooltip);
														}
													});

											for (int k = 0; k < item.getFEValBabelSynsetListLength(); k++) 
											{
												String tmp = item.getFEValBabelSynsetID(k);
												synsetComboBox.getItems().add(tmp);
											}
											;
											synsetComboBox.getSelectionModel().select(item.getFEValBabelSynsetID());																						
											vb.getChildren().addAll(feVal, synsetComboBox, affinityScore);
										} else {
											vb.getChildren().addAll(feVal);
										}
										setGraphic(vb);
									}
								}
							};
							return cell;
						}
					});
			col.setStyle("-fx-alignment: CENTER;");
			col.setPrefWidth(160);
			table.getColumns().add(col);
		}
		TableColumn<Frame, String> scoreCol = new TableColumn<Frame, String>("Score");
		scoreCol.setCellValueFactory(new Callback<CellDataFeatures<Frame, String>, ObservableValue<String>>() {
		     public ObservableValue<String> call(CellDataFeatures<Frame, String> param) {
		         return new ReadOnlyObjectWrapper(param.getValue().getFrameScoreStr());
		     }
		  });
		scoreCol.setStyle("-fx-alignment: CENTER;");
		scoreCol.setPrefWidth(150);
		table.getColumns().add(scoreCol);
		table.setItems(data);
		table.setOnMouseClicked((MouseEvent event) -> {
		    if (event.getClickCount() >= 1) {
		    	Frame selectedFrame = table.getSelectionModel().getSelectedItem();
		    	if(selectedFrame != null)
		    	{
		    		ulrResult.setText(selectedFrame.getULR());
		    	}
		    }
		});
		newTab.setContent(table);
		tabPane.getTabs().add(newTab);
		return newTab;
	}
	
	private String getParsingError()
	{
		String result = null;
		try (BufferedReader br = 
				new BufferedReader(new FileReader("scripts/prolog/ape/tmp/serialized_drs_fact.txt"))) 
		{
			String line = br.readLine();
			if(line != null)
			{
				result = line;
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		return result;
	}
	
	private void setSentenceParsingQuery(String sentence)
	{
		try (BufferedWriter bw = 
				new BufferedWriter(new FileWriter("scripts/prolog/ape/query/qparse.pl"))) 
		{
			String newSentence = sentence.replace("'", "\\'");
			bw.write("parse_and_serialize_main :- parse_and_serialize('" + newSentence + "').");
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	private void setFrameExtractionQuery(int index)
	{
		try (BufferedWriter bw = 
				new BufferedWriter(new FileWriter("scripts/prolog/ape/query/qframe_extraction.pl"))) 
		{
			bw.write("extract_frame_and_serialize_main :- extract_frame_and_serialize(" + index + ").");
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	private void initSentenceTable(TableView<String> sentenceTable, ObservableList<String> sentenceData, double width, TabPane tabPane) //throws Exception
	{
		TableColumn<String, String> col = new TableColumn<String, String>();
		col.setCellValueFactory(new Callback<CellDataFeatures<String, String>, ObservableValue<String>>() {
		     public ObservableValue<String> call(CellDataFeatures<String, String> param) {
		         return new ReadOnlyObjectWrapper(param.getValue());
		     }
		  });
		col.setStyle("-fx-alignment: CENTER;");
		col.setPrefWidth(1100);
		sentenceTable.getColumns().add(col);
		sentenceTable.getSelectionModel().selectedItemProperty().addListener((observableValue, oldValue, newValue) -> {
            if (sentenceTable.getSelectionModel().getSelectedItem() != null) 
            {
            	tabPane.getTabs().clear();
            	ulrResult.setText("");
            	int index =  sentenceTable.getSelectionModel().getSelectedIndex() + 1;
            	setFrameExtractionQuery(index);
            	PrologConnector.ExecutePrologQuery();
            	
            	long startTime = System.currentTimeMillis();
            	ArrayList<Frame> frameList = FrameExtractor.GetFrameExtractionResult();
            	long stopTime = System.currentTimeMillis();
                long elapsedTime = stopTime - startTime;
              
            	for (Frame frame : frameList) 
            	{
    				Tab tab = ExistFrameTab(tabPane, frame);
    				if (tab == null) {
    					addNewTab(tabPane, frame);
    				} else {
    					AddRowToTab(tab, frame);
    				}
    			}
            	serializeScore(null, elapsedTime, frameList, false);
            	serializeTopResult(null, frameList, false);
            	serializeTopResultWithRankOnly(null, frameList, false);
            }
        });
	}
	
	private List<String> sentenceToList(String sentence)
	{
		List<String> list = new ArrayList<String>();
		String[] result = sentence.trim().split("\\.");
		for(String sent : result)
		{
			if(sent.length() != 0)
			{
				list.add(sent.trim() + ".");
			}
		}
		return list;
	}
	
	private void serializeScore(String sentence, long elapsedTime, ArrayList<Frame> frameList,
			boolean isAppend) //throws IOException
	{
		try (BufferedWriter bw = new BufferedWriter(new FileWriter("resources/scores/score.txt", isAppend))) 
		{
			if(sentence != null)
			{
				bw.write(sentence + " ");
			}
			bw.write("(Total time(s): " + elapsedTime/1000 + ")\n");
		    for(Frame frame : frameList)
		    {
		    	bw.write(frame.print());
		    }
		}
		catch (IOException x) 
		{
		      System.err.println(x);
		      x.printStackTrace();
		      //throw x;
		}
	}
	
	private void serializeTopResult(String sentence, ArrayList<Frame> frameList, boolean isAppend) //throws IOException
	{
		try (BufferedWriter bw = new BufferedWriter(new FileWriter("resources/scores/result.pl", isAppend))) 
		{
			if(sentence != null)
			{
				for(Frame frame : frameList)
			    {
			    	bw.write("result('" + sentence.replace("'", "\\'") + "',");
			    	bw.write(frame.getTopResult());
			    	bw.write(").\n");
			    }
			}
			else
			{
				for(Frame frame : frameList)
			    {
			    	bw.write("result(");
			    	bw.write(frame.getTopResult());
			    	bw.write(").\n");
			    }
			}
		}
		catch (IOException x) 
		{
		      System.err.println(x);
		      x.printStackTrace();
		      //throw x;
		}
	}
	
	private void serializeTopResultWithRankOnly(String sentence, ArrayList<Frame> frameList, boolean isAppend) //throws IOException
	{
		try (BufferedWriter bw = new BufferedWriter(new FileWriter("resources/scores/result_rank.pl", isAppend))) 
		{
			int count = 0;
			if(sentence != null)
			{
				for(Frame frame : frameList)
			    {
					count++;
			    	bw.write("result('" + sentence.replace("'", "\\'") + "',");
			    	bw.write(frame.getTopResultWithoutScore() + count);
			    	bw.write(").\n");
			    }
			}
			else
			{
				for(Frame frame : frameList)
			    {
					count++;
			    	bw.write("result(");
			    	bw.write(frame.getTopResultWithoutScore() + count);
			    	bw.write(").\n");
			    }
			}
		}
		catch (IOException x) 
		{
		      System.err.println(x);
		      x.printStackTrace();
		      //throw x;
		}
	}
	
	private void batchProcessing()
	{
		try (BufferedReader br = new BufferedReader(new FileReader("resources/batch/batch_query.txt"))) 
		{
			int count = 0;
			String sentence;
			long totalElapsedTime = 0;
			while((sentence = br.readLine())!=null)
			{
				QueryProcessing.ClearVarWordIndexSet();
				if(QueryProcessing.IsQuery(sentence))
				{
					QueryProcessing.PreProcessQuery(sentence);
				}
				setSentenceParsingQuery(sentence.replace("$", ""));
				PrologConnector.ExecutePrologQuery();
				if(QueryProcessing.IsQuery(sentence))
				{
					QueryProcessing.ExtractImplicitVar();
				}
				String result = getParsingError();
				if(result == null)
				{
					count++;
					setFrameExtractionQuery(1);
	            	PrologConnector.ExecutePrologQuery();
	            	
	            	long startTime = System.currentTimeMillis();
	            	ArrayList<Frame> frameList = FrameExtractor.GetFrameExtractionResult();
	            	long stopTime = System.currentTimeMillis();
	                long elapsedTime = stopTime - startTime;
	                totalElapsedTime += elapsedTime;
	                if(count == 1)
	                {
	                	serializeScore(sentence, elapsedTime, frameList, false);
	                	serializeTopResult(sentence, frameList, false);
	                	serializeTopResultWithRankOnly(sentence, frameList, false);
	                }
	                else
	                {
	                	serializeScore(sentence, elapsedTime, frameList, true);
	                	serializeTopResult(sentence, frameList, true);
	                	serializeTopResultWithRankOnly(sentence, frameList, true);
	                }
				}
			}
			System.out.println("Total time (s): " + totalElapsedTime);
		}
		catch (IOException x) 
		{
		      System.err.println(x);
			x.printStackTrace();
		}
	}
}
