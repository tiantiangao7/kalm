package main.java.edu.stonybrook.cs.batch;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import main.java.edu.stonybrook.cs.frame.Frame;
import main.java.edu.stonybrook.cs.frame.FrameElement;
import main.java.edu.stonybrook.cs.frameextraction.FrameExtractor;
import main.java.edu.stonybrook.cs.util.PrologConnector;

public class Batch {

	private String getParsingError() {
		String result = null;
		try (BufferedReader br = new BufferedReader(new FileReader("scripts/prolog/ape/tmp/serialized_drs_fact.txt"))) {
			String line = br.readLine();
			if (line != null) {
				result = line;
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		return result;
	}

	private void setSentenceParsingQuery(String sentence) {
		try (BufferedWriter bw = new BufferedWriter(new FileWriter("scripts/prolog/ape/query/qparse.pl"))) {
			String newSentence = sentence.replace("'", "\\'");
			bw.write("?-parse_and_serialize('" + newSentence + "').");
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private void setFrameExtractionQuery(int index) {
		try (BufferedWriter bw = new BufferedWriter(new FileWriter("scripts/prolog/ape/query/qframe_extraction.pl"))) {
			bw.write("?-extract_frame_and_serialize(" + index + ").");
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private List<String> sentenceToList(String sentence) {
		List<String> list = new ArrayList<String>();
		String[] result = sentence.trim().split("\\.");
		for (String sent : result) {
			if (sent.length() != 0) {
				list.add(sent.trim() + ".");
			}
		}
		return list;
	}

	private void serializeScore(String sentence, long elapsedTime, ArrayList<Frame> frameList, boolean isAppend) {
		try (BufferedWriter bw = new BufferedWriter(new FileWriter("resources/scores/score.txt", isAppend))) {
			if (sentence != null) {
				bw.write(sentence + " ");
			}
			bw.write("(Total time(s): " + elapsedTime / 1000 + ")\n");
			for (Frame frame : frameList) {
				bw.write(frame.print());
			}
		} catch (IOException x) {
			System.err.println(x);
		}
	}

	private void batchProcessing() {
		try (BufferedReader br = new BufferedReader(new FileReader("resources/batch/batch.txt"))) {
			int count = 0;
			String sentence;
			long totalElapsedTime = 0;
			while ((sentence = br.readLine()) != null) {
				setSentenceParsingQuery(sentence);
				PrologConnector.ExecutePrologQuery();
				String result = getParsingError();
				if (result == null) {
					count++;
					setFrameExtractionQuery(1);
					PrologConnector.ExecutePrologQuery();

					long startTime = System.currentTimeMillis();
					ArrayList<Frame> frameList = FrameExtractor.GetFrameExtractionResult();
					long stopTime = System.currentTimeMillis();
					long elapsedTime = stopTime - startTime;
					totalElapsedTime += elapsedTime;
					if (count == 1) {
						serializeScore(sentence, elapsedTime, frameList, false);
					} else {
						serializeScore(sentence, elapsedTime, frameList, true);
					}
				}
			}
			System.out.println("Total time (s): " + totalElapsedTime);
		} catch (IOException x) {
			System.err.println(x);
		}
	}
	
	public static void main(String[] args){
		Batch b = new Batch();
		b.batchProcessing();
	}
}
