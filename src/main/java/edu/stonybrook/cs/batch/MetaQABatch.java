package main.java.edu.stonybrook.cs.batch;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;

import main.java.edu.stonybrook.cs.util.PrologConnector;

public class MetaQABatch {
	private void setSentenceParsingQuery(String sentence) {
		try (BufferedWriter bw = new BufferedWriter(new FileWriter("scripts/prolog/ape/query/qparse.pl"))) {
			bw.write("parse_and_serialize_main :- parse_and_serialize('" + sentence + "').");
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	private String formVarInequalities(String ruleBody) {
		HashMap<String, ArrayList<String>> map = new HashMap<String, ArrayList<String>>();
		Pattern pattern = Pattern.compile("movie\\(.+?\\)");
		Matcher matcher = pattern.matcher(ruleBody);
		while (matcher.find())
		{
		    String predicate = matcher.group(0);
		    predicate = predicate.substring(6, predicate.length()-1);
		    String[] ele = predicate.split(",");
		    if (!map.containsKey(ele[0])) {
		    	ArrayList<String> temp = new ArrayList<String>();
		    	temp.add(ele[1]);
		    	map.put(ele[0], temp);
		    } else {
		    	ArrayList<String> temp = map.get(ele[0]);
		    	boolean contain = false;
		    	for (String s : temp) {
		    		if (s.equals(ele[1])) {
		    			contain = true;
		    			break;
		    		}
		    	}
		    	if (!contain) {
		    		temp.add(ele[1]);
		    	}
		    }
		    if (!map.containsKey(ele[2])) {
		    	ArrayList<String> temp = new ArrayList<String>();
		    	temp.add(ele[3]);
		    	map.put(ele[2], temp);
		    } else {
		    	ArrayList<String> temp = map.get(ele[2]);
		    	boolean contain = false;
		    	for (String s : temp) {
		    		if (s.equals(ele[3])) {
		    			contain = true;
		    			break;
		    		}
		    	}
		    	if (!contain) {
		    		temp.add(ele[3]);
		    	}
		    }	    
		}
		String result = "";
		for (String key : map.keySet()) {
			ArrayList<String> temp = map.get(key);
			for (int i = 0; i < temp.size(); i++) {
				for (int j = i + 1; j < temp.size(); j++) {
					if (!temp.get(i).startsWith("W") && !temp.get(j).startsWith("W")) {
						continue;
					} else {
						result += "," + temp.get(i) + " \\= " + temp.get(j);
					}
				}
			}
		}
		return result;
	}
	
	private void composeQuery(int id) {
		String ruleHead = "q" + id + "(";
		try (BufferedReader br = new BufferedReader(new FileReader("scripts/prolog/ape/tmp/implicit_var.txt"))) {
			String var;
			boolean found = false;
			while ((var = br.readLine()) != null) {
				found = true;
				ruleHead = ruleHead + "W" + var.substring(3) + ",";
			}
			if (found) {
				ruleHead = ruleHead.substring(0, ruleHead.length() - 1) + ")";
			} else {
				ruleHead = null;
			}
		} catch (IOException x) {
		}
		
		String ruleBody = null;
		try (BufferedReader br = new BufferedReader(new FileReader("scripts/prolog/ape/metaqa/metaqa_query.txt"))) {
			ruleBody = br.readLine();
			if (ruleBody == null || ruleBody.equals("?-.")) {
				ruleBody = null;
			} else {
				ruleBody = ruleBody.substring(2, ruleBody.length()-1);
				String varList = formVarInequalities(ruleBody);
				ruleBody += varList;
			}
		} catch (IOException x) {
		}
		
		if (id == 1) {
			try (BufferedWriter bw 
					= new BufferedWriter(new FileWriter("scripts/prolog/ape/metaqa/eval/metaqa_query.pl"))) {			
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		
		try (BufferedWriter bw = new BufferedWriter(new FileWriter("scripts/prolog/ape/metaqa/eval/metaqa_query.pl", true))) {
			if (ruleHead == null || ruleBody == null) {
				System.out.println("failed parsing: " + id);
				bw.write("?-write('query " + id +" failed.\\n').\n");
			} else {
				bw.write(ruleHead + ":-" + ruleBody + ".\n");
				bw.write("?-" + ruleHead + ".\n");
				if (StringUtils.countMatches(ruleBody, "movie(") < 3) {
					System.out.println("missing lvp: " + id);
				}
				if (StringUtils.countMatches(ruleBody, "movie(") > 3) {
					System.out.println("redundant lvp: " + id);
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	private void batchProcessing() {
		try (BufferedReader br = new BufferedReader(new FileReader("resources/metaqa/metaqa_query.txt"))) {
			String sentence;
			long startTime = System.currentTimeMillis();
			int id = 0;
			while ((sentence = br.readLine()) != null) {
				// set prolog query for the metaqa query sentence.
				setSentenceParsingQuery(sentence);
				PrologConnector.ExecutePrologQuery();
				id++;
				composeQuery(id);
			}
			long stopTime = System.currentTimeMillis();
			long elapsedTime = stopTime - startTime;
			System.out.println("Total time (s): " + elapsedTime/1000);
		} catch (IOException x) {
			System.err.println(x);
			x.printStackTrace();
		}
	}
	
	public static void main(String[] args) {
		MetaQABatch batch = new MetaQABatch();
		batch.batchProcessing();
	}

}
