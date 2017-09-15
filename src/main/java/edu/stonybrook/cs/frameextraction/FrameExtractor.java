package main.java.edu.stonybrook.cs.frameextraction;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import main.java.edu.stonybrook.cs.frame.Frame;
import main.java.edu.stonybrook.cs.frame.FrameElement;
import main.java.edu.stonybrook.cs.frame.LoadFrame;

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
		    		fe.setFEVal(val[1].replace("-", " "), val[3]);
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
		    for(Frame frame : frameList)
		    {
		    	for(FrameElement fe : frame.getAllFrameElements())
		    	{
		    		fe.setFEValBabelSynsetIDList();
		    		fe.setFEValBabelSynsetID();
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
		
		for(Frame frame : frameList)
		{
			frame.resetScore();
		}
		return frameList;
	}
}
