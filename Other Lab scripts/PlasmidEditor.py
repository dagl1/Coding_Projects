# -*- coding: utf-8 -*-
"""
Created on Mon Mar 13 12:49:06 2023

@author: Jelle Bonthuis
"""
import os
from Bio import SeqIO
from Bio.Seq import Seq
from Bio.SeqRecord import SeqRecord
from Bio.SeqFeature import SeqFeature
from Bio.SeqFeature import SeqFeature, SimpleLocation
import copy
import re

#### User input ####



#### End User input ###

def findOrCreatePathsFromParentDirectory(path):
	os.chdir(path)

	DNApath = path + "DNASequences/"
	PENTRpath = path + "pENTR/"
	PDESTpath = path + "pDEST/"
	PCDNA5path = path + "PCDNA/"
	paths = [DNApath,PENTRpath,PDESTpath,PCDNA5path]
	try:
		for path1 in paths:
			os.chdir(path1)
	except:
		print("One or more paths were unavailable and are being created")
		for path1 in paths:
			try: 
				os.mkdir(path1)
				print("created %s, please move relevant files to this path and rerun the program"%path1)
			except: 
				print("Could not create %s, it already exist"%path1 )
		
	
	return paths

def parseFastaOrGenbankFilesInDirectory(path):
	os.chdir(path)
	files = os.listdir(path)
	Sequences =  {}
	for file in files:
		sequence = ''
		if file.endswith('.txt'):
			with open(file, 'r') as f:
				sequence = f.read()
		elif file.endswith('.gb'):	
			with open(file) as DNASequence:
				for record in SeqIO.parse(DNASequence, "genbank"):
					sequence = str(record.seq)
		# Get the file name without extension
		filename = os.path.splitext(file)[0]
		if filename in Sequences:
			print(f"You added several file formats of the same gene ({filename}), taking the first one and ignoring all others.")
		else:
		# Add the sequence to the dictionary with the file name as the key
			Sequences[filename] = sequence

	return Sequences

def check_feature(feature):
    try:
        if feature.location.operator == "join":
            return False
    except:
        return True

def performBPreaction(Sequences,path,pDONR_Type = "221", plasmidType = "pENTR"):
	os.chdir(path)
	files = os.listdir(path)
	#print(Sequences)

	if plasmidType== 'pCDNA5':
		pDONR_Type ="pENTR"
		reactionType = "LR" 
		text = "pcdna5_frt_to_"
		types = ["N-ready","C-ready"]

	elif plasmidType == "pENTR":
		reactionType = "BP"
		text = "pdonr"
		types = ["N-ready","C-ready"]

	try:
		with open("pdonr" + pDONR_Type + ".gb", "r") as pDONR:
			for record in SeqIO.parse(pDONR, "genbank"):				
				record1 = copy.deepcopy(record)
				for sequence in Sequences.items():

					types = ["N-ready","C-ready"]
					for terminalType in types:
						tempPDONR = pDONR
						record = copy.deepcopy(record1)
						#print(record.features)
						for count,feature in enumerate(record.features):
							if feature.qualifiers['label'] ==["attP1"]:
								toRemoveBegin = feature.location.start
								#seq[feature.location.start : feature.location.end].reverse_complement()
								#print((feature.location.start))
							elif feature.qualifiers['label'] ==["attP2"]:
								#print((feature.location.start))
								toRemoveEnd = feature.location.end

						def check_feature(feature):
							try:	
								if (feature.location.end < toRemoveBegin) or (feature.location.start > toRemoveEnd):
									return False
								else:
									return True
							except:
								return True

						featuresToAddOrigin = [feature for feature in [feature for feature in record.features if not check_feature(feature)]]
						#print(featuresToAddOrigin)
						if terminalType == "N-ready":
							sequenceTemp = sequence[1][3:]

						elif terminalType == "C-ready":
							sequenceTemp = sequence[1][:len(sequence[1])-3]
						if plasmidType == "pENTR":
							ATTL1Sequence = "CAAATAATGATTTTATTTTGACTGATAGTGACCTGTTCGTTGCAACAMATTGATGAGCAATGCTTTTTTATAATGCCAACTTTGTACAAAAAAGCAGGCT"
							ATTL2Sequence = "ACCCAGCTTTCTTGTACAAAGTTGGCATTATAAGAAAGCATTGCTTATCAATTTGTTGCAACGAACAGGTCACTATCAGTCAAAATAAAATCATTATTTG"
							sequenceTemp = ATTL1Sequence +   sequenceTemp   + ATTL2Sequence
							labels = ["attP1","attP2"]
						lengthOfInsertedSequence = len(sequenceTemp)


						toRemoveBegin = 0
						toRemoveEnd   = 0 
						for count,feature in enumerate(record.features):
							if feature.qualifiers['label'] ==["attP1"]:
								toRemoveBegin = feature.location.start
								#seq[feature.location.start : feature.location.end].reverse_complement()
								#print((feature.location.start))
							elif feature.qualifiers['label'] ==["attP2"]:
								#print((feature.location.start))
								toRemoveEnd = feature.location.end
						### If the part to remove goes through the end 		

						if toRemoveBegin > toRemoveEnd:
							print('Part to remove of plasmid goes through beginning of plasmid, please remake the pENTR plasmid so that the ATTP1 and ATTP2 are both on one side of the 0th nucleotide')
						
						lengthOfRemovedPart = toRemoveEnd - toRemoveBegin

						lengthModifier = lengthOfInsertedSequence - lengthOfRemovedPart # if positive, inserted sequence was bigger and all features need their location to change 

						newSequence = record.seq[:toRemoveBegin] + sequenceTemp + record.seq[toRemoveEnd:]
						
						newFeatures = [] 
						#for count,feature in enumerate(record.features):
						
						#### filters out any features that are in the to-be-removed part
						#print(record.features)
						#featuresToAddOrigin = [feature for feature in [feature for feature in record.features if not check_feature(feature)]]
						print('d\n')
						#print(featuresToAddOrigin)
						print('ddd\n')
						featuresToAdd = featuresToAddOrigin [:]
						featuresToAddTemp = []
						#print(featuresToAdd)
						for count, feature in enumerate(featuresToAddOrigin):
							if feature.location.end >toRemoveBegin:
								newStart = feature.location.start + lengthModifier 
								newEnd = feature.location.end + lengthModifier
								oldStrand = feature.location.strand
								if newStart > len(newSequence):

									newStart = newStart - len(newSequence)
									newEnd = newEnd - len(newSequence)


								elif newEnd > len(newSequence):
									newFeature = feature
									tempLocation = SimpleLocation(0,newEnd-len(newSequence),strand = oldStrand)
									newEnd = len(newSequence)
									newFeature.location = tempLocation
									featuresToAddTemp.append(newFeature)
							else:
								newStart = feature.location.start
								newEnd = feature.location.end
								oldStrand = feature.location.strand

									#print(newFeature)



							#breakUpFeatureIntoTwoFeatures
							#print(newStart,newEnd)
							newLocation = SimpleLocation(newStart,newEnd, strand = oldStrand)
							feature.location = newLocation 
							#print(feature.location)
							#print(newLocation)
							featuresToAddTemp.append(feature) 
							#print(feature)
							#print(feature)
						
						featuresToAdd =featuresToAddTemp [:]

						if terminalType == "N-ready":
							tag = "n" 
							codon = "start"
							pass
						elif terminalType == "C-ready":
							tag = "c"
							codon = "stop"
							pass

							
						#@oldFeatures  = [feature for feature in [feature for feature in record.features if check_feature(feature)]]
						#print((record.features))

						gene = sequence[0]#get the name of the original DNA file minus GB
						genename = gene + " minus " + codon
						newFeatures = createNewFeatures(toRemoveBegin, genename, reaction = "BP", sequence = newSequence)
						#print(featuresToAdd)
						#print(newFeatures)
						for feature in newFeatures:
							added = False
							for i in range(len(featuresToAdd)):
								if feature.location.start< featuresToAdd[i].location.start:
									featuresToAdd.insert(i,feature)
									added = True
									break
							if not added:
								featuresToAdd.append(feature)
						#print(featuresToAdd)
						print("\n\n\n\n")		

						#print(oldFeatures)
						#print('\ndddd')
						name= plasmidType+"-" + gene + "-" + tag	
						newRecord = SeqRecord(
							Seq(newSequence),
							name=name,
							description="%s %s of %s. %s codon is removed."% (terminalType, plasmidType, gene, codon),
							features = featuresToAdd,
							annotations={"molecule_type": "DNA","topology": "circular" }
						)
						print(newRecord.features)
						SeqIO.write(newRecord, name+".gb", "genbank")

	except Exception as e:
		print("Could perform code with pdonr. Full error: "+ str(e))


def createNewFeatures(beginSlice, genename, reaction,sequence):
	if reaction == "BP":
		ATTL1Sequence = "CAAATAATGATTTTATTTTGACTGATAGTGACCTGTTCGTTGCAACAMATTGATGAGCAATGCTTTTTTATAATGCCAACTTTGTACAAAAAAGCAGGCT"
		ATTL2Sequence = "ACCCAGCTTTCTTGTACAAAGTTGGCATTATAAGAAAGCATTGCTTATCAATTTGTTGCAACGAACAGGTCACTATCAGTCAAAATAAAATCATTATTTG"
		primers = [ATTL1Sequence,ATTL2Sequence]
		primerNames = ["ATTL1", "ATTL2"]
		colours = [['#c6c9d1'],['#FF6103'], ['#31849b']]
	
	if reaction == "LR":
		ATTB1Sequence = "CAAGTTTGTACAAAAAAGCAGGCT"
		ATTB2Sequence = "ACCCAGCTTTCTTGTACAAAGTGG"
		primers = [ATTB1Sequence,ATTB2Sequence]
		primerNames = ["ATTB1", "ATTB2"]
		colours = [['#c6c9d1'],['#FF6103'], ['#31849b']]



	#print(sequence.find(primers[1]))
	beginSlice = [sequence.find(primers[0]), sequence.find(primers[0])+len(primers[0]),primerNames[0], colours[0], "gateway"]
	middleSlice = [sequence.find(primers[0])+len(primers[0]),sequence.find(primers[1]),genename,colours[1], "gene"]
	endSlice = [sequence.find(primers[1]),sequence.find(primers[1])+len(primers[1]),primerNames[1], colours[2], "gateway"]
	slices = [beginSlice,middleSlice,endSlice]
	#print(endSlice[0],endSlice[1])
	
	#print(endSlice[0],endSlice[1])
	#print("d\n\n\n\n")
	features = []
	for slice1 in slices:
		print(primers[0])
		feature = SeqFeature(SimpleLocation(slice1[0], slice1[1], strand=1), type = slice1[4])
		feature.qualifiers ={'label': [slice1[2]], 'ApEinfo_revcolor': slice1[3], 'ApEinfo_fwdcolor': slice1[3] }
		features += [feature]
		
	return features
	#print(sequence[beginSlice:endSLice])

def performLRreaction(Sequences,path, outputpath, plasmidType = "pCDNA5"):
	os.chdir(path)
	files = os.listdir(path)
	#print(Sequences)

	if plasmidType== 'pCDNA5':
		pDONR_Type ="pENTR"
		reactionType = "LR" 
		text = "pcdna5_frt_to_"
	pattern = r'^pcdna5_frt_to_(gfp|miniturbo)_(c|n)(_.+)?\.gb$'
	for file in files:
		os.chdir(path)
		print(file)
		if file.endswith('.gb'):
			match = re.match(pattern, file)
			print(file)
			if match:
				types = [match.group(2).upper()+"-ready"]
				tagType = match.group(1)
			else:
				continue
			#try:
			with open(file, "r") as pDEST:
				

				for record in SeqIO.parse(pDEST, "genbank"):				
					record1 = copy.deepcopy(record)
					for sequence in Sequences.items():	
						os.chdir(path)
						for terminalType in types:

							record = copy.deepcopy(record1)

							for count,feature in enumerate(record.features):

								if 'label' in feature.qualifiers.keys():

									if  feature.qualifiers['label'] ==["attR1"]:
										toRemoveBegin = feature.location.start
										#seq[feature.location.start : feature.location.end].reverse_complement()
										#print((feature.location.start))
									elif feature.qualifiers['label'] ==["attR2"]:
										#print((feature.location.start))
										toRemoveEnd = feature.location.end

							def check_feature(feature):
								try:	
									if (feature.location.end < toRemoveBegin) or (feature.location.start > toRemoveEnd):
										return False
									else:
										return True
								except:
									return True

							featuresToAddOrigin = [feature for feature in [feature for feature in record.features if not check_feature(feature)]]

							if terminalType == "N-ready":
								sequenceTemp = sequence[1][3:]

							elif terminalType == "C-ready":
								sequenceTemp = sequence[1][:len(sequence[1])-3]

							if plasmidType== 'pCDNA5':
								ATTB1Sequence = "CAAGTTTGTACAAAAAAGCAGGCT"
								ATTB2Sequence = "ACCCAGCTTTCTTGTACAAAGTGG"
								sequenceTemp = ATTB1Sequence +   sequenceTemp   + ATTB2Sequence
								labels = ["ATTL1","ATTL2"]
								print(sequenceTemp)
							lengthOfInsertedSequence = len(sequenceTemp)
							


							# toRemoveBegin = 0
							# toRemoveEnd   = 0 
							# for count,feature in enumerate(record.features):
							# 	if feature.qualifiers['label'] ==["attR1"]:
							# 		toRemoveBegin = feature.location.start
							# 		#seq[feature.location.start : feature.location.end].reverse_complement()
							# 		#print((feature.location.start))
							# 	elif feature.qualifiers['label'] ==["attR2"]:
							# 		#print((feature.location.start))
							# 		toRemoveEnd = feature.location.end
							### If the part to remove goes through the end 		

							if toRemoveBegin > toRemoveEnd:
								print('Part to remove of plasmid goes through beginning of plasmid, please remake the pENTR plasmid so that the ATTP1 and ATTP2 are both on one side of the 0th nucleotide')
							
							lengthOfRemovedPart = toRemoveEnd - toRemoveBegin

							lengthModifier = lengthOfInsertedSequence - lengthOfRemovedPart # if positive, inserted sequence was bigger and all features need their location to change 

							newSequence = record.seq[:toRemoveBegin] + sequenceTemp + record.seq[toRemoveEnd:]
							#print(newSequence)
							newFeatures = [] 
							#for count,feature in enumerate(record.features):
							
							#### filters out any features that are in the to-be-removed part
							#print(record.features)
							#featuresToAddOrigin = [feature for feature in [feature for feature in record.features if not check_feature(feature)]]
							print('d\n')
							#print(featuresToAddOrigin)
							print('ddd\n')
							featuresToAdd = featuresToAddOrigin [:]
							featuresToAddTemp = []
							#print(featuresToAdd)
							for count, feature in enumerate(featuresToAddOrigin):
								if feature.location.end >toRemoveBegin:
									newStart = feature.location.start + lengthModifier 

									newEnd = feature.location.end + lengthModifier
									oldStrand = feature.location.strand
									if newStart > len(newSequence):

										newStart = newStart - len(newSequence)
										newEnd = newEnd - len(newSequence)


									elif newEnd > len(newSequence):
										newFeature = feature

										tempLocation = SimpleLocation(0,newEnd-len(newSequence),strand = oldStrand)
										newEnd = len(newSequence)
										newFeature.location = tempLocation
										featuresToAddTemp.append(newFeature)
								else:
									newStart = feature.location.start
									newEnd = feature.location.end
									oldStrand = feature.location.strand

										#print(newFeature)



								#breakUpFeatureIntoTwoFeatures
								#print(newStart,newEnd)
								#print(newStart)
								newLocation = SimpleLocation(newStart,newEnd, strand = oldStrand)
								feature.location = newLocation 
								#print(feature.location)
								#print(newLocation)
								featuresToAddTemp.append(feature) 
								#print(feature)
								#print(feature)
							
							featuresToAdd =featuresToAddTemp [:]

							if terminalType == "N-ready":
								tag = "n" 
								codon = "start"
								pass
							elif terminalType == "C-ready":
								tag = "c"
								codon = "stop"
								pass

								
							#@oldFeatures  = [feature for feature in [feature for feature in record.features if check_feature(feature)]]
							#print((record.features))

							gene = sequence[0]#get the name of the original DNA file minus GB
							genename = gene + " minus " + codon
							newFeatures = createNewFeatures(toRemoveBegin, genename, reaction = reactionType , sequence = newSequence)
							#print(featuresToAdd)
							#print(newFeatures)
							for feature in newFeatures:
								added = False
								for i in range(len(featuresToAdd)):
									if feature.location.start< featuresToAdd[i].location.start:
										featuresToAdd.insert(i,feature)
										added = True
										break
								if not added:
									featuresToAdd.append(feature)
							#print(featuresToAdd)
							print("\n\n\n\n")		

							#print(oldFeatures)
							#print('\ndddd')
							os.chdir(outputpath)
							name= plasmidType+"-" + gene + "-" + tagType+"-" + tag	
							newRecord = SeqRecord(
								Seq(newSequence),
								name=name,
								description="%s %s of %s. %s codon is removed."% (terminalType, plasmidType, gene, codon),
								features = featuresToAdd,
								annotations={"molecule_type": "DNA","topology": "circular" }
							)
							print(newRecord.features)
							SeqIO.write(newRecord, name+".gb", "genbank")

			# except Exception as e:
			# 	print("Could perform code with pdonr. Full error: "+ str(e))




def insertDNASequenceTo_PENTR(paths):
	DNApath = paths[0]
	DNASequences = parseFastaOrGenbankFilesInDirectory(DNApath)

	PENTRpath = paths[1]
	performBPreaction(DNASequences,PENTRpath,pDONR_Type= "221",plasmidType = "pENTR")
	PDESTpath = paths[2]
	PCDNA5path = paths[3]
	performLRreaction(DNASequences,PDESTpath,PCDNA5path)

	pass

def createN_Or_C_PENTR_From_PENTR():
	pass

def insertN_And_C_PENTR_Into_PDEST():
	pass

def main():
	path = "E:/python/PlasmidEditor/"
	paths = findOrCreatePathsFromParentDirectory(path)
	insertDNASequenceTo_PENTR(paths)
	pass

main()

# path = "E:/python/PlasmidEditor/DNASequences/"
# os.chdir(path)

# with open("mideas-long Full Sequence.gb") as DestinationPlasmid:
# 	for record in SeqIO.parse(DestinationPlasmid, "genbank"):
# 		print(record.seq)
# 		#print(record.features[15].qualifiers)


# my_Seq = Seq("TTGAAAACCTGAATGTGAGAGTCAGTCAAGGATAGT")
# my_SeqFeature = SeqFeature(SimpleLocation(5, 18, strand=-1), type="gene")


# record = SeqRecord(
#     Seq(my_Seq),
#     name="HokC",
#     description="toxic membrane protein, small",
#     features = [my_SeqFeature]
# )
# print(my_gene.qualifiers["gene"])
# print(my_gene.qualifiers["product"])
# SeqFeature(SimpleLocation(ExactPosition(1886), ExactPosition(2011), strand=1), type='misc_feature', qualifiers=...),
