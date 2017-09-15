package main.java.edu.stonybrook.cs.thread;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.PriorityQueue;
import java.util.Set;

public class ParallelInverseSemanticScoreThread extends Thread{
	/*
	private EdgeNode root = null;
	private EdgeNode fromNode = null;
	private String[] toList = null;
	private HashMap<String, Integer> toListMap = new HashMap<String, Integer>();
	
	public ParallelInverseSemanticScoreThread(EdgeNode root, EdgeNode fromNode, String[] toList)
	{
		this.root = root;
		this.fromNode = fromNode;
		this.toList = toList;
		int index = 0;
		for(String sid : this.toList)
		{
			toListMap.put(sid, index);
			index++;
		}
	}
	
	@Override
	public void run()
	{
		Set<String> set = new HashSet<String>();
		HashMap<String, EdgeNode> nodeMap = new HashMap<String, EdgeNode>();
		PriorityQueue<EdgeNode> queue = 
				new PriorityQueue<EdgeNode>(100, (a,b) -> (int)Math.ceil(b.curScore - a.curScore));
		
		nodeMap.put(root.edgeNodeSynsetID, root);
		set.add(root.edgeNodeSynsetID);
		queue.add(root);
		
		while (!queue.isEmpty()) {
			EdgeNode parentNode = queue.poll();
	//		double globalVal = BabelNetShareResource.getGlobalVal(); 
			
			if(parentNode.curScore < globalVal && parentNode.curScore > 0.00000001 )
			{
				continue;
			}
			
			ArrayList<EdgeNode> parents = null;
			if(parentNode.edgeNodeSynsetID.equals(root.edgeNodeSynsetID))
			{
				parents = new ArrayList<EdgeNode>();
				parents.add(fromNode);
			}
			else
			{
				parents = BabelNetShareResource.GetEdgeNodes(parentNode.edgeNodeSynsetID);
			}
			for (EdgeNode targetNode : parents) {
				assert targetNode.edgeNodeSynsetID != null;
				assert targetNode.edgeWeight > 0.0;
				assert targetNode.edgeType != null;
				assert targetNode.parent != null;
				assert parentNode.edgeNodeSynsetID == targetNode.parent;
				
			//	globalVal = BabelNetShareResource.getGlobalVal();
				if (set.contains(targetNode.edgeNodeSynsetID)||targetNode.edgeNodeSynsetID.equals("bn:02248101n")) 
				{
					continue;
				} 
				else if(toListMap.containsKey(targetNode.edgeNodeSynsetID)) 
				{
					targetNode.SemConnectionNum = 
							BabelNetShareResource.GetSemanticConnectionNum(targetNode.edgeNodeSynsetID);
					targetNode.totalWeightedCount = targetNode.SemConnectionNum
							* targetNode.edgeWeight + nodeMap.get(parentNode.edgeNodeSynsetID).totalWeightedCount;
					ArrayList<String> path = BabelNetShareResource.getPath(nodeMap, targetNode);
					targetNode.curScore = BabelNetShareResource.computeSemanticScore(targetNode.totalWeightedCount, path);
				//	BabelNetShareResource.setGlobalVal(targetNode.curScore);
					String tmpAncestorPath = BabelNetShareResource.printPath(nodeMap, targetNode);
					//BabelNetShareResource.setScore(toListMap.get(targetNode.edgeNodeSynsetID), targetNode.curScore,
					//		tmpAncestorPath);
				} 
				else 
				{
					targetNode.SemConnectionNum = 
							BabelNetShareResource.GetSemanticConnectionNum(targetNode.edgeNodeSynsetID);
					targetNode.totalWeightedCount = targetNode.SemConnectionNum
							* targetNode.edgeWeight + nodeMap.get(parentNode.edgeNodeSynsetID).totalWeightedCount;
					ArrayList<String> path = BabelNetShareResource.getPath(nodeMap, targetNode);
					targetNode.curScore = BabelNetShareResource.computeSemanticScore(targetNode.totalWeightedCount, path);
					if (targetNode.curScore >= globalVal)
					{
						queue.add(targetNode);
						set.add(targetNode.edgeNodeSynsetID);
						nodeMap.put(targetNode.edgeNodeSynsetID, targetNode);
					}
				}
			}
		}
	}
	*/
}
