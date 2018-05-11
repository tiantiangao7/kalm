package main.java.edu.stonybrook.cs.tool;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import it.uniroma1.lcl.babelnet.BabelNet;
import it.uniroma1.lcl.babelnet.BabelSynset;
import it.uniroma1.lcl.babelnet.BabelSynsetID;
import it.uniroma1.lcl.babelnet.InvalidBabelSynsetIDException;
import main.java.edu.stonybrook.cs.thread.BabelNetShareResource;
import main.java.edu.stonybrook.cs.util.BabelNetConnector;

public class AnalysisGenerator {
	private static String dir = "resources/scores/";
	private static String inputFileName = "score.txt";
	private static String outputFileName = "analysis.txt";
	
	public static void ReadRawResults()
	{
		try (BufferedReader br = new BufferedReader(new FileReader(dir + inputFileName))) {
			BufferedWriter bw = new BufferedWriter(new FileWriter(dir + outputFileName));
			String line = null;
			while((line = br.readLine()) != null) {
				line = line.trim();
				if(!IsPath(line))
				{
					bw.write(line + "\n");
				}
				else
				{
					PrintAnalysis(bw, line);
				}
			}
			bw.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	private static boolean IsPath(String line)
	{
		Matcher m = Pattern.compile("^bn:[0-9]{8}n:.*").matcher(line);
		if(m.find())
		{
			return true;
		}
		else
		{
			return false;
		}
	}
	
	private static void PrintAnalysis(BufferedWriter bw, String line)
	{
		String[] parts = new String[2];
		
		Matcher m = Pattern.compile("^(bn:[0-9]{8}n:.*)\\s+([0-9]+\\.[0-9]+)$").matcher(line);
		while(m.find())
		{
			parts[0] = m.group(1);
			parts[1] = m.group(2);
		}	
		
		String[] elements = parts[0].split("-");
		try {
			bw.write("\tscore: " + parts[1] + "\n");
			bw.write("\t[BEGIN]" + "\n");
			for(int i = 0; i < elements.length; i++)
			{
				if(i%2 == 0)
				{
					String[] vals = elements[i].split(":");
					String synset = vals[0] + ":" + vals[1];
					String numerator = vals[2];
					bw.write("\t" + synset + " " + numerator + " " + (int)Math.pow(BabelNetShareResource.GetSemanticConnectionNum(synset),2) + " " + GetGlossBySynsetID(synset) + "\n");
				}
				else
				{
					bw.write("\t" + elements[i] + "\n");
				}
			}
			bw.write("\t[END]" + "\n");
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	private static String GetGlossBySynsetID(String sid)
	{		
		BabelNet bn = BabelNet.getInstance();
		BabelSynset by = null;
		String gloss = null;
		try {
			by = bn.getSynset(new BabelSynsetID(sid));
			gloss = BabelNetConnector.getMainGloss(by);
		} catch (IOException e) {
			e.printStackTrace();
		} catch (InvalidBabelSynsetIDException e) {
			e.printStackTrace();
		}
		return gloss;
	}
	
	public static void main(String[] args){
		ReadRawResults();
	}
}
