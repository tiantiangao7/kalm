package main.java.edu.stonybrook.cs.frameextraction;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

import main.java.edu.stonybrook.cs.frame.Frame;
import main.java.edu.stonybrook.cs.frame.FrameElement;
import main.java.edu.stonybrook.cs.frame.LoadFrame;
import main.java.edu.stonybrook.cs.query.QueryProcessing;

public class FrameExtractor {
	private static String FEQueryInput = "scripts/prolog/ape/tmp/query.pl";
	private static String FEQueryOutput = "scripts/prolog/ape/tmp/query_output.txt";
	
	public static ArrayList<Frame> GetFrameExtractionResult()
	{
		ArrayList<Frame> frameList = new ArrayList<Frame>();
		
		try (BufferedReader br = new BufferedReader(new FileReader(FEQueryOutput))) 
		{
			HashSet<String> hashSet = new HashSet<String>();
			List<Thread> threadList = new ArrayList<Thread>();
		    String line;	
		    while ((line = br.readLine()) != null) {
		    	if(line.contains("=") == false)
		    	{
		    		Frame frame = LoadFrame.getFrame(line);
		    		assert frame != null;
		    		frameList.add(frame);
		    	}
		    	else
		    	{
		    		String[] val = line.split("=");
		    		Frame frame = frameList.get(frameList.size()-1);
		    		FrameElement fe = frame.getFrameElementbyName(val[0]);
		    		assert fe != null;
		    		fe.setFEVal(val[1].replace("-", " "), val[2], val[3], val[4]);
		    		List<Thread> tmpThreadList = fe.createSematicScoreComputationThreads();
		    		if(!hashSet.contains(fe.getFrameElementKey()) && tmpThreadList != null)
		    		{
		    			hashSet.add(fe.getFrameElementKey());
		    			for(Thread t : tmpThreadList)
		    			{
		    				threadList.add(t);
		    			}
		    		}		    		
		    		//fe.setFEValBabelSynsetIDList();
		    		//fe.setFEValBabelSynsetID();
		    	}
		    }
		    for(Thread t : threadList)
		    {
		    	t.start();
		    }
		    for(Thread t : threadList)
		    {					
		    	t.join();
		    }
		    
		    HashMap<String, Double> tmpScoreMap = new HashMap<String, Double>();
		    for(Frame frame : frameList)
		    {
		    	for(FrameElement fe : frame.getAllFrameElements())
		    	{
		    		fe.setFEValBabelSynsetIDList();
		    		fe.setFEValBabelSynsetID();
		    		String feVal = fe.getFEVal();
		    		Double score = fe.getMainSynsetAffinityScore();
		    		if(tmpScoreMap.containsKey(feVal))
		    		{
		    			if(score > tmpScoreMap.get(feVal))
		    			{
		    				tmpScoreMap.put(feVal, score);
		    			}
		    		}
		    		else
		    		{
		    			tmpScoreMap.put(feVal, score);
		    		}
		    	}
		    }
		    
		    for(Frame frame : frameList)
		    {
		    	for(FrameElement fe : frame.getAllFrameElements())
		    	{
		    		if(fe.ExistExactMatch())
		    		{
		    			//to-do: what if feval is not in tmpScoreMap?
		    			Double score = tmpScoreMap.get(fe.getFEVal()); 
		    			if(fe.getFEValBabelSynsetListLength() > 0)
		    			{
		    				fe.SetExactMatch(score + 0.5);
		    			}
		    			else
		    			{
		    				fe.SetExactMatch(2.0);
		    			}
		    			fe.setFEValBabelSynsetID();
		    		}
		    	}
		    }
		}
		catch (IOException x) 
		{
		      System.err.println(x);
		}
		catch (InterruptedException e) {
			e.printStackTrace();
		}
		
		// Remove if this change affects the accuracy
		ComputeCoefficientVal(frameList);
		
		for(Frame frame : frameList)
		{
			frame.resetFrameScore();
		}
		
		Collections.sort(frameList, new Comparator<Frame>(){
		     public int compare(Frame f1, Frame f2){
		    	 if(f1.getFrameScore() > f2.getFrameScore())
		    	 {
		    		 return -1;
		    	 }
		    	 else if(f1.getFrameScore() < f2.getFrameScore())
		    	 {
		    		 return 1;
		    	 }
		    	 else
		    	 {
		    		 return 0;
		    	 }
		     }
		});
		
		ArrayList<Integer> subsumedAnswerList = removeSubsumedFrames(frameList);
		for(int i = subsumedAnswerList.size() - 1; i >= 0; i--)
		{
			int index = subsumedAnswerList.get(i);
			frameList.remove(index);
		}
		return frameList;
	}
	
	private static void ComputeCoefficientVal(ArrayList<Frame> frameList)
	{
		for(Frame frame : frameList)
		{
			int max = frame.getNonEmptyFENum();
			for(Frame tempFrame : frameList)
			{
				if(tempFrame.getFrameName().equals(frame.getFrameName()) && tempFrame.getNonEmptyFENum() > max)
				{
					max = tempFrame.getNonEmptyFENum();
				}
			}
			frame.setCoefficientVal(1.0*frame.getNonEmptyFENum()/max);
		}
	}
	
	private static ArrayList<Integer> removeSubsumedFrames(ArrayList<Frame> frameList)
	{
		ArrayList<Integer> subsumedAnswerList = new ArrayList<Integer>();
		for(int i = 0; i < frameList.size(); i++)
		{
			for(int j = 0; j < frameList.size(); j++)
			{
				if(i == j)
				{
					continue;
				}
				
				Frame f1 = frameList.get(i);
				Frame f2 = frameList.get(j);
				if(!f1.getFrameName().equals(f2.getFrameName()))
				{
					continue;
				}
				ArrayList<FrameElement> l1 = f1.getAllFrameElements();
				ArrayList<FrameElement> l2 = f2.getAllFrameElements();
				assert l1.size() == l2.size();
				boolean isSubsumed = true;
				for(int k = 0; k < l1.size(); k++)
				{
					FrameElement fe1 = l1.get(k);
					FrameElement fe2 = l2.get(k);
					assert fe1.getFEName().equals(fe2.getFEName());
					if(fe1.getFEVal() == null && fe2.getFEVal() == null)
					{
						continue;
					}
					if(fe1.getFEVal() == null || fe2.getFEVal() == null)
					{
						isSubsumed = false;
						break;
					}				
					if(fe1.getFEVal().equals(fe2.getFEVal()))
					{
						if(QueryProcessing.IsInVarWordIndexSet(fe1.getFEValWordIndex())
								&& !QueryProcessing.IsInVarWordIndexSet(fe2.getFEValWordIndex()))
						{
							isSubsumed = false;
							break;
						}
						else
						{
							continue;
						}
					}
					if(!fe1.getFEVal().equals(fe2.getFEVal()))
					{
						if(!QueryProcessing.IsInVarWordIndexSet(fe1.getFEValWordIndex())
								&& QueryProcessing.IsInVarWordIndexSet(fe2.getFEValWordIndex()))
						{
							continue;
						}
						else
						{
							isSubsumed = false;
							break;
						}
					}
				}
				if(isSubsumed == true)
				{
					subsumedAnswerList.add(i);
				}
			}
		}		
		return subsumedAnswerList;
	}
}
