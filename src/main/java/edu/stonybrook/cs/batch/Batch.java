package main.java.edu.stonybrook.cs.batch;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import main.java.edu.stonybrook.cs.fpparser.FrameDescriptionPredicate;
import main.java.edu.stonybrook.cs.fpparser.SemanticLinkAddition;
import main.java.edu.stonybrook.cs.fpparser.SemanticLinkOverride;
import main.java.edu.stonybrook.cs.fpparser.SemanticScoreParameters;
import main.java.edu.stonybrook.cs.fpparser.SynsetOverride;
import main.java.edu.stonybrook.cs.frame.Frame;
import main.java.edu.stonybrook.cs.frameextraction.FrameExtractor;
import main.java.edu.stonybrook.cs.query.QueryProcessing;
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
			bw.write("parse_and_serialize_main :- parse_and_serialize('" + newSentence + "').");
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private void setFrameExtractionQuery(int index) {
		try (BufferedWriter bw = new BufferedWriter(new FileWriter("scripts/prolog/ape/query/qframe_extraction.pl"))) {
			bw.write("extract_frame_and_serialize_main :- extract_frame_and_serialize(" + index + ").");
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private void serializeScore(int num, String sentence, long elapsedTime, ArrayList<Frame> frameList, boolean isAppend) {
		try (BufferedWriter bw = new BufferedWriter(new FileWriter("resources/scores/score.txt", isAppend))) {
			if (sentence != null) {
				bw.write(num + ". " + sentence + " ");
			}
			bw.write("(Total time(s): " + elapsedTime / 1000 + ")\n");
			for (Frame frame : frameList) {
				bw.write(frame.print());
			}
		} catch (IOException x) {
			System.err.println(x);
			x.printStackTrace();
		}
	}

	private void serializeTopResult(String sentence, ArrayList<Frame> frameList, boolean isAppend)
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
		}
	}
	
	private void serializeTopResultWithRankOnly(String sentence, ArrayList<Frame> frameList, boolean isAppend)
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
		}
	}
	
	private void batchProcessing() {
		try (BufferedReader br = new BufferedReader(new FileReader("resources/batch/batch_query.txt"))) {
			int count = 0;
			String sentence;
			long totalElapsedTime = 0;
			while ((sentence = br.readLine()) != null) {
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
						serializeScore(count, sentence, elapsedTime, frameList, false);
						serializeTopResult(sentence, frameList, false);
						serializeTopResultWithRankOnly(sentence, frameList, false);
					} else {
						serializeScore(count, sentence, elapsedTime, frameList, true);
						serializeTopResult(sentence, frameList, true);
						serializeTopResultWithRankOnly(sentence, frameList, true);
					}
				}
			}
			System.out.println("Total time (s): " + totalElapsedTime);
		} catch (IOException x) {
			System.err.println(x);
			x.printStackTrace();
		}
	}
	
	public static void main(String[] args){
		Batch b = new Batch();
		FrameDescriptionPredicate.Parse();
		SemanticLinkOverride.initialize();
		SemanticScoreParameters.initialize();
		SynsetOverride.initialize();
		SemanticLinkAddition.initialize();
		b.batchProcessing();
	}
}
