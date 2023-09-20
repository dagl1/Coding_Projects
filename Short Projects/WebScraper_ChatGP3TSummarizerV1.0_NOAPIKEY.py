from pymed import PubMed
import pandas as pd
from datetime import datetime, timedelta
import json 
import os
import sys
import logging, logging.handlers
from xml.etree.ElementTree import ElementTree
from xml.etree.ElementTree import Element
from xml.etree.ElementTree import SubElement
import xml.etree.ElementTree as ET 
import re
import inspect
#from BeautifulSoup import BeautifulStoneSoup
from itertools import islice
#from tokenizers import Tokenizer
#from transformers import GPT2Tokenizer, GPT2Model
import tiktoken
import math
#from DistAnnot.process import TimedSemaphore
import openai
openai.api_key = "REMOVED"

def setCurrentDate():
	CurrentdateTemp = datetime.today()# - timedelta(days=1)
	CurrentdateTemp = str(CurrentdateTemp).split(" ")[0].replace("-", "/")
	Currentdate =CurrentdateTemp
	print(Currentdate)
	return Currentdate


def readDictionaryFromFile(path):

	if "LatestDictToSearch.txt" not in os.listdir(path):
		SearchDateAndQueryDict = dict()
	else: 
		with open("LatestDictToSearch.txt") as file1:
			SearchDateAndQueryDict = json.loads(file1.read())
	return SearchDateAndQueryDict


def readQueryTermsFile(path):
	if "QueryTerms.txt" not in os.listdir(path):
		raise Exception( "No QueryTerms.txt found, is it placed in the given path/directory? Please make sure the spelling is correct")
	else: 
		with open("QueryTerms.txt") as file1:
			QueryTerms = file1.read().splitlines()
			print(QueryTerms)
	return QueryTerms


def updateTermToDict(term, mydict,Currentdate):
	mydict.update({term:Currentdate})

	
def saveDictionaryToFile(mydict):

	with open('LatestDictToSearch.txt', 'w') as convert_file:
		convert_file.write(json.dumps(mydict))


def createDirectories(path):
	try:
		path = path + "xml_short results/"
		os.mkdir(path)
	except:
		pass

def loadDataFrame(path):
	try: 
		df = pd.read_csv(path+"AllHits.csv")
	except: 
		df = pd.DataFrame(columns=["searchTerm","article_id","title", "keywords", "publication_date", "abstract", "PMCID"])
	return df


def performSearch(df,mydict,myqueries, Currentdate,path):
	pubmed = PubMed(tool="MyPubmedArticleScraper", email= "jellebonthuis@hotmail.com")
	path = path + "xml_short results/"

	### add in folders per query
	for QueryTerm in myqueries:
		os.chdir(path)
		NewPath = path + str(QueryTerm) + "/"
		try:	
			print(NewPath)
			os.mkdir(NewPath)
			os.chdir(NewPath)

		except:
			pass

		os.chdir(NewPath)
		if QueryTerm in mydict:
			LastsearchDate = mydict[QueryTerm]
		else:
			LastsearchDate = "2000/01/01"

		updateTermToDict(QueryTerm,mydict,Currentdate)
		
		Query = QueryTerm + " AND " +LastsearchDate[0:12] +  ":" + Currentdate[0:12] + "[pdat]" #LastsearchDate[0:12]
		print(Query)

		results = pubmed.query(Query, max_results = 500)
		currentAmountOfArticles = len(df.searchTerm)

		df = accessingResults(df,QueryTerm,results,NewPath)
		amountOfArticleAdded = len(df.searchTerm) - currentAmountOfArticles
		if amountOfArticleAdded>0:
			createCSVWithLatestHits(df.tail(amountOfArticleAdded)	,NewPath)

	return df


def accessingResults(df, searchTerm,results,path):
	os.chdir(path)
	#isTrue= True
	
	for article in results:
		
		### Checks if pubmed id was already in database, in which case we dont do anyting with the search result
		res = df.isin([article.pubmed_id]).any().any()
		if(res):
			pass
		else:

			if hasattr(article, 'xml'):
				emptyRow = pd.Series([None,None,None,None,None,None,None],index = ["searchTerm","article_id","title", "keywords", "publication_date", "abstract", "PMCID"])
				
				df = df.append(      emptyRow, ignore_index=True   )
				
				### This part of the code can save xmls but doesnt access OA for full text XML
				
				tree = ElementTree(article.xml)
				name = str(article.pubmed_id.split()[0])+".xml"
				with open(name, 'wb') as f:
					tree.write(f)

				### Saves some of the results into the df
				df['article_id'].iloc[-1] = article.pubmed_id.split()[0]
				df['title'].iloc[-1] = article.title
				if article.keywords:
					if None in article.keywords:
						article.keywords.remove(None)
					
					df['keywords'].iloc[-1]='", "'.join(article.keywords)
				df['publication_date'].iloc[-1] = article.publication_date
				df['abstract'].iloc[-1] = article.abstract
				df['searchTerm'].iloc[-1] = searchTerm
				df['PMCID'].iloc[-1] = 0
		
	return df


def saveDataFrame(df,path):
	df.to_csv(path+"AllHits.csv",index = False)

def createCSVWithLatestHits(df,path):
	df.to_csv(path+"LatestHits.csv",index = False)


def saveCommaSeperatedListOf_PMIDs(df,QueryTerms):
	with open('article_ids.txt', 'w') as f:	
		count2 = 1		
		for count, ID in enumerate(df.article_id):
			if ID != 0 and ID != "0":
				print(count)
				if count2 != 1:
					print(ID)
					text = ","+str(ID)
					f.write(text)
				else: 
					text = str(ID)
					f.write(text)
				count2 += 1

	for query in QueryTerms:
		originalQuery= query
		query = query.replace("(", "_")
		query = query.replace(")", "_")
		query = query.replace(" ", "_")
		with open('article_ids%s.txt'%query, 'w') as f:
			count2 = 1
			for count, ID in enumerate(df.PMCID):

				if df.searchTerm[count] == originalQuery and ID != 0 and ID != "0":
					#print(count)
					if count2 != 1:
						print(ID)
						text = ","+str(ID)
						f.write(text)
					else: 
						text = str(ID)
						f.write(text)
					count2 += 1


def convertPMIDToPMCID(df,path):
	
	from Bio import Entrez

	# Set email address for PubMed API
	Entrez.email = 'jellebonthuis@hotmail.com'

	# Define the list of PMIDs to convert
	pmids = df.article_id 
	
    # Loop through the PMIDs and use Entrez E-utilities to retrieve PMCIDs
	#pmcids = []
	#counterForSaving = 0
	for count, pmid in enumerate(pmids):
		try:
			if df.PMCID[count] == 0:
				handle = Entrez.efetch(db='pubmed', id=str(pmid), retmode='xml')
				record = Entrez.read(handle)
				for article_id in record['PubmedArticle'][0]['PubmedData']['ArticleIdList']:
					pmcid = 0
					if article_id.attributes.get('IdType') == 'pmc':
						pmcid = article_id
						print(pmcid)
						break
				df.PMCID[count] = pmcid
		except:
			df.PMCID[count] = "connectionError"
			saveDataFrame(df,path)
	return df

def saveCommaSeperatedListOf_PMCIDs(df,QueryTerms):
	with open('PMCIDs.txt', 'w') as f:
		count2 = 1
		for count, ID in enumerate(df.PMCID):

			if ID != 0 and ID != "0":
				if count2 != 1:
					print(ID)
					text = ","+str(ID)
					f.write(text)
				else: 
					text = str(ID)
					f.write(text)
				count2 +=1

	for query in QueryTerms:
		originalQuery= query
		query = query.replace("(", "_")
		query = query.replace(")", "_")
		query = query.replace(" ", "_")
		with open('PMCIDs%s.txt'%query, 'w') as f:
			count2 = 1
			for count, ID in enumerate(df.PMCID):

				print()
				if df.searchTerm[count] == originalQuery and ID != 0 and ID != "0":
					print(count)
					if count2 != 1:
						print(ID)
						text = ","+str(ID)
						f.write(text)
					else: 
						text = str(ID)
						f.write(text)
					count2 +=1

def runPyGetPapersPerQueryTerm(path, QueryTerms,nameOfRun):
	 
	for query in QueryTerms:
		print(query)
		query = query.replace("(", "_")
		query = query.replace(")", "_")
		query = query.replace(" ", "_")
		name = nameOfRun+query
		print('pygetpapers --terms %sPMCIDs%s.txt -k 10 -s -x -o "%s"'%(path, query, query))
		#print(name)
		stream = os.popen('pygetpapers --terms %sPMCIDs%s.txt -k 10 -s -x -o "%s"'%(path, query, query))
		output = stream.read()
		#print(output)

def openAndProcessXMLsForAllQueryTerms(QueryTerms,path,nameOfRun):
	#print(os.listdir())
	directories=[d for d in os.listdir(os.getcwd()) if os.path.isdir(d)]

	for directory in directories:
		if directory != "xml_short results" :
			os.chdir(path+directory+"/")
			innerDirectories = [d for d in os.listdir(os.getcwd()) if os.path.isdir(d)]
			for innerDirectory in innerDirectories:
				#print(innerDirectory)
				#if innerDirectory == "PMC9946804":
				PMCPath = path+directory+ "/"+innerDirectory+"/fulltext.xml"
				os.chdir(path+directory+ "/"+innerDirectory+"/")

				ListWithChaptersOnlyText = accessXMLFile(PMCPath) #takes path
				ListWithChaptersOnlyText = manipulateTextPerSection(ListWithChaptersOnlyText)
				saveListWithChaptersIntoTextFile(ListWithChaptersOnlyText,path+directory+"/"+innerDirectory + "/" )

	
	pass

def accessXMLFile(path):
	# Pass the path of the xml document 
	ListWithChaptersOnlyText = []
	try: 

		tree = ET.parse(path) # PMC9482474 PMC9351884

	# get the parent tag 
		root = tree.getroot()

		ListWithChaptersOnlyText = getAbstractFromXML(root)
		ListWithChaptersOnlyText = getTextWithoutFiguresFromXML(root,ListWithChaptersOnlyText)
	except:
		print("could not find xml at %s "%path)

	return ListWithChaptersOnlyText

def getAbstractFromXML(root):
	ListWithChaptersOnlyText = []
	for child in root:
		if child.tag == "front":
			for section in child:
				if section.find('abstract') != None:
					abstractSection = section.find('abstract')

					for thing in abstractSection.iter():
						if thing.tag == 'abstract':
							sectionName = 'abstract'

							ListWithChaptersOnlyText +=  ["<<<%s>>>"% (sectionName)+("".join(section.itertext()))+"\n"]

	return ListWithChaptersOnlyText

def getTextWithoutFiguresFromXML(root,ListWithChaptersOnlyText):
	FullChaptersWithFiguresList =[]
	for child in root:
		if child.tag == "body":
			for section in child:
				listWithSectionsFigures = []
				sectionName = section.get('sec-type') 
				if sectionName == "materials|methods" or sectionName == 'subjects|methods':
					sectionName = "methods"
				if sectionName == "conclusion":
					sectionName = "conclusions"
				if sectionName == "ethics-statement":
					sectionName = "content"
				if sectionName == None:
					if "".join(section.itertext())[:30].lower().find("introduction") != -1:
						sectionName = "intro"
					elif "".join(section.itertext())[:30].lower().find("Highlight") != -1:
						sectionName = "highlight"
					elif "".join(section.itertext())[:30].lower().find("background") != -1:
						sectionName = "intro"
					elif "".join(section.itertext())[:30].lower().find("method") != -1:
						sectionName = "methods"
					elif "".join(section.itertext())[:30].lower().find("statis") != -1:
						sectionName = "stats"	
					elif "".join(section.itertext())[:30].lower().find("result") != -1:
						sectionName = "results"
					elif "".join(section.itertext())[:30].lower().find("limitation") != -1:
						sectionName = "limitations"
					elif "".join(section.itertext())[:30].lower().find("discussion") != -1:
						sectionName = "discussion"
					elif "".join(section.itertext())[:30].lower().find("conclusion") != -1:
						sectionName = "conclusions"
					elif "".join(section.itertext())[:30].lower().find("author") != -1:
						sectionName = "author-contributions"
					elif "".join(section.itertext())[:30].lower().find("fund") != -1:
						sectionName = "Fund information"	
					elif "".join(section.itertext())[:30].lower().find("consent") != -1:
						sectionName = "consent"	
					elif "".join(section.itertext())[:30].lower().find("ethics") != -1:
						sectionName = "consent"	
					elif "".join(section.itertext())[:30].lower().find("ethics-statement") != -1:
						sectionName = "consent"	
					

				
				ListWithChaptersOnlyText +=  ["<<<%s>>>"% (sectionName)+("".join(section.itertext()))+"\n"] ### add <<< >>> tag with sectionName
				for thing in section.iter():
					
					if thing.tag == "fig" or thing.tag == "table":
						figuretext = "".join(thing.itertext())
						listWithSectionsFigures.append(figuretext)

				FullChaptersWithFiguresList +=	[listWithSectionsFigures]	

	for figuresSection in FullChaptersWithFiguresList:
		if len(figuresSection)>0:
			for figure in figuresSection: 
				for count,textSection in enumerate(ListWithChaptersOnlyText):
					editedtext = textSection.replace(figure, "")
					ListWithChaptersOnlyText[count] = editedtext	
	return ListWithChaptersOnlyText


def manipulateTextPerSection(ListWithChaptersOnlyText):

	for count,sec in enumerate(ListWithChaptersOnlyText):
		sectionName = identifySectionName(sec)
		section = cleanUpTextFromArticle(sec,sectionName)
		ListWithChaptersOnlyText[count] = section
	return ListWithChaptersOnlyText



def saveListWithChaptersIntoTextFile(ListWithChaptersOnlyText,path):
	os.chdir(path)
	#print(path)
	try:
		with open('FullText.txt', 'w',encoding='utf8') as f:
			for section in ListWithChaptersOnlyText:
				f.write(section)
	except Exception as e:
		print("FullText.Txt was open, could not edit \n full error: %s"%e)



def identifySectionName(text):
	### Find <<< >>> at beginning of each text section to identify it, we added this tag in the access XML function 

	BeginningLocation = text.find("<<<")
	EndingLocation = text.find(">>>")

	if BeginningLocation == -1 or EndingLocation == -1:
		print("could not find <<< >>> tag in text")
		sectionName = 'unknown'
	else:
		sectionName = text[BeginningLocation+3:EndingLocation] 

	return sectionName

def removeBackslashCode(text):
	a = text.find('\\')
	while a!=-1:
		
		b = text[a:].find(' ')
		print(text[a:].find(' '))
		text = text[:a]+text[:b]

		a = text.find('\\')

	return text

def removeSquareBracketReferences(text):
	# Match square brackets with variable digits inside
	pattern = r"\[\d+(,\d+)*\]"

	# Replace matches with empty string
	text = re.sub(pattern, "", text)

	return text
def removeEmptyBracketsAndParentheis(text):
	text = re.sub(r'\(\s*\)', '', text)
	text = re.sub(r'\[\s*\]', '', text)
	return text

def removeAPAReferences(text):
	ref_pattern = r"\((?:[A-Z][a-z]+(?:-[A-Z][a-z]+)?(?:\s&\s)?)+,?\s\d{4}[a-z]?(?:[;,]?\s(?:and\s)?(?:[A-Z][a-z]+(?:-[A-Z][a-z]+)?(?:\s&\s)?)+,?\s\d{4}[a-z]?)*\)"
	text = re.sub(ref_pattern, "", text)
	ref_pattern = r"\([A-Za-z\s&]+,\s\d{4}[a-z]?(?:;\s[A-Za-z\s&]+,\s\d{4}[a-z]?)*\)"
	text = re.sub(ref_pattern, "", text)
	ref_pattern = r'\((?:[\w \.&]+\, )+[0-9]{4}\)'
	text = re.sub(ref_pattern, "", text)

	### Probably the onlky one necessary, but not going to change the magic regular expressions
	ref_pattern = r'\((?:\s*\.\s*\d{4}\s*)\)'
	text = re.sub(ref_pattern, '', text)
	#print(re.search(ref_pattern,text))
	return text

def removeNumberReferences(text):
	pattern = r"\b\d+\b[.,]?"
	text = re.sub(pattern, "", text)
	text = re.sub(r',\s*,', ',', text)
	text = re.sub(r",\s*(?:,|\n)", ",", text)
	return text

def removeEmail(text):
	text = re.sub(r'\S+@\S+', '', text)
	return text

def removeWebsites(text):
	text = re.sub(r'http\S+', '', text)
	return text

def findAndRemoveReferencesByEtAl(text): # find any references by checking for et al.
	text = re.sub(r'\bal\.\d{4}\b', '', text)
	return text

def removeDepartmentOfInAbstract(text):
	text = re.sub(r'\d+Department\s+of\s+\S+\s*,\s*\S+\s*,\s*\S+\s*\n*', '', text)
	return text
def findBeginningOfAbstractBasedOnWordDotWord(text): #generally the parsing e.r or any of such things (not perfect as there are acronyms like that)
	patterns = [
	r'\b\w+\.(?![0-9])[A-Z]',
	r'\b.+?\.(?![0-9])[A-Z]'
	r'\b\w+\)(?![0-9])[A-Z]',
	r'\b\w+\](?![0-9])[A-Z]',
	]
	matches = []
	for pattern in patterns:
		matches.extend(re.finditer(pattern, text))
	matches = [match for match in matches if match]
	matches = sorted(matches, key=lambda match: match.span()[1])
	if matches:
		smallest_match = matches[0]
		text = text[smallest_match.span()[1]-1:]


	return text

def findlastOccurenceOfLicenceInAbstract(text):
	a = text.rfind('license')
	if license != 0:

		text = text[a:]
	else:
		text = findBeginningOfAbstractBasedOnWordDotWord(text)
	return text

def removeArticleNumberNotationAtEndOfAbstract(text):
	# find first occurrence of 3 numbers slash 3 numbers pattern
	match = re.search(r"\d{3}/\d{3}", text)

	if match:
    # remove everything after the first occurrence of the pattern
		text = text[:match.start()]
	return text

def findBackgroundInAbstract(text):
	listWithTerms = ['background', 'abstract', 'introduction']
	arbitrarilyLargeNumber = 1000000
	index = arbitrarilyLargeNumber # 
	lenOfWord = 0
	EndingLocation = text.find(">>>")
	for term in listWithTerms:
		number = text[EndingLocation+3:].lower().find(term)
		if number> 0 and number<index:
			index = number
			lenOfWord = len(term)
	if index != arbitrarilyLargeNumber:
		text = text[index+lenOfWord:]
	return text

def remove_newlines(text):
    text = text.replace('\n', ' ')
    text = text.replace('\\n', ' ')
    text = text.replace('  ', ' ')
    text = text.replace('  ', ' ')
    return text

def cleanUpTextFromArticle(text,sectionName):
	### Find links and remove
	### find [13] 
	### Find empty () and []
	### get rid of 

	text = removeSquareBracketReferences(text)
	text = removeAPAReferences(text)
	text = removeNumberReferences(text)
	text = text.replace('\n', ' ')

	if sectionName == "abstract":
		#text = removeDepartmentOfInAbstract(text)
		text = findlastOccurenceOfLicenceInAbstract(text)
		text = removeArticleNumberNotationAtEndOfAbstract(text)
		text = findBackgroundInAbstract(text)
		text = ""#"<<<abstract>>>" + text
		

		#print(index)
		#print(text)
	elif sectionName == "intro":
		pass
	elif sectionName == "highlight":
		pass
	elif sectionName == "discussion":
		pass
	elif sectionName == "methods":
		text = ""
	elif sectionName == "results":
		pass
	elif sectionName == "conclusions":
		pass
	elif sectionName == "limitations":
		pass
	elif sectionName == "author-contributions":
		text = ""		
		sectionName = 'removed'		
	elif sectionName == "data-availability":
		text = ""	
		sectionName = 'removed'
	elif sectionName == "supplementary-material":
		text = ""	
		sectionName = 'removed'
	elif sectionName == "COI-statement":
		text = ""
		sectionName = 'removed'
	elif sectionName == "Fund information":
		text = ""	
		sectionName = 'removed'
	elif sectionName == "consent":
		text = ""
		sectionName = 'removed'
	elif sectionName == "consent":
		text = ""
		sectionName = 'removed'
	elif sectionName == 'content':
		text = ""
			
	else:
		print("Can't identify section name")
	text = remove_newlines(text)
	text = removeEmail(text)
	text = removeWebsites(text)
	text = findAndRemoveReferencesByEtAl(text)
	text = removeBackslashCode(text)
	text = removeEmptyBracketsAndParentheis(text)	
	return text


def openTextFilesForProcessing(QueryTerms,path,nameOfRun):
	os.chdir(path)
	directories=[d for d in os.listdir(os.getcwd()) if os.path.isdir(d)]
	for directory in directories:
		#print(directory)
		if directory != "xml_short results":
			os.chdir(path+directory+"/")
			#print(path+directory+"/")
			innerDirectories = [d for d in os.listdir(os.getcwd()) if os.path.isdir(d)]
			for innerDirectory in innerDirectories:
				#print(innerDirectory)
				if innerDirectory == "PMC9756953":
					print('d')
					PMCPath = path+directory+ "/"+innerDirectory
					os.chdir(path+directory+ "/"+innerDirectory+"/")
					#print(os.getcwd())
					try:
						with open("FullText.txt",encoding='utf8') as file1:
							text = file1.read()
							#print(text)
							#print(text)
							text = processSingleFile(text)
							saveTextFileAfterProcessing(text,PMCPath)

					except Exception as e: 
						print("Could not open FullText.txt for processing \n error: %s"% e)


def saveTextFileAfterProcessing(text,path):
	os.chdir(path)
	try:
		with open('ReadyToUseText.txt', 'w',encoding='utf8') as f:
			f.write(text)
			
	except Exception as e:
		print("ReadyToUseText.Txt was open, or another error occured; could not edit \n full error: %s"%e)


def callGPT3AndOutputNewText(promptText, preText, highlight, Ttype,max_tokens = 1000):
	if Ttype == "clean":
		PromptInstruction = 'This is a part or chapter of a scientific article, you are preparing it for summarization, please clean up any unnecessary punctuation, remove references and generally reduce the amount of tokens in this text without changing the meaning: '
	
	elif Ttype == "sum1":
		PromptInstruction = 'This is a chopped up part of chapter of a scientific article, you are summarzing this part for scientists so that the chapter can be summarized in full, keep relevant information and be concise, summary length should be between 50 and 70 percent of input text, please only return the text after "to be summarized": '

	elif Ttype == "highlight":
		PromptInstruction = "This is a part or chapter of a scientific article, create a max 80 word highlight/summary of this text that can be used for context in the next parts: "

	else:
		print("please add a valid type variable : clean, sum1, or highlight")

	if calculateTokensInPieceOfText(PromptInstruction + "\nHighlight: \n\n" + highlight  + "\nPrevious text for context: \n" + preText + "\n\nThe text to be summarized:  \n\n"+ promptText + "\n")>1000:
		print("found pieces too long: "+ PromptInstruction + "\nHighlight: \n\n" + highlight  + "\nPrevious text for context: \n" + preText + "\n\nThe text to be summarized:  \n\n"+ promptText + "\n")
		print(calculateTokensInPieceOfText(PromptInstruction + "\nHighlight: \n\n" + highlight  + "\nPrevious text for context: \n" + preText + "\n\nThe text to be summarized:  \n\n"+ promptText + "\n"))
	else:

		gpt_prompt = PromptInstruction + "\nHighlight:" + highlight  + "\nPrevious text for context: \n" + preText + "\n\nThe text to be summarized:  \n\n"+ promptText + "\n"
	#print(gpt_prompt)



	response = openai.Completion.create(
	engine="text-curie-001",
	prompt=gpt_prompt,
	temperature=0.5,
	max_tokens=max_tokens,
	top_p=1.0,
	frequency_penalty=0.0,
	presence_penalty=0.0
	)
	#summarizeFullChapterPrompt = 'This is a chapter of a scientific article, you are summarzing the chapter for scientists in the field, summarize as many relevant things and try to get the size at least 20 percent smaller: '
	return response['choices'][0]['text']

def processSingleFile(text, max_tokens = 1000, max_prompt_tokens = 300):

	sections = splitTextToSections(text) # and so we ensure that the size is always large enough for us to fit in other information and also still get a response
	totalLengthOfAllSections = calculateTokensInPieceOfText(text)
	HighLight = ''
	OutPutText = ''
	CleanedUpText = ''
	Test = True
	for count2, sectionFull in enumerate(sections):
		sectionFull = split_into_many(sectionFull)
		CleanedUpText += "<<<tag>>>" 
		for count1, section in enumerate(sectionFull):
			#if Test == True and count1 == 0:
			CleanedUpText += callGPT3AndOutputNewText(section, preText = "", highlight = "", Ttype = "clean",max_tokens = 900) + "\n"
				#Test = False

	print(CleanedUpText)
	CleanedUpTextSections = splitTextToSections(CleanedUpText)
	for count2, sectionFull in enumerate(CleanedUpTextSections):
		sectionFull = split_into_many(sectionFull)
		for count1, section in enumerate(sectionFull):	
			if count1>0:
				preText = "".join(sectionFull[count1-1].split(". ")[::-1][0:7][::-1])#[0]
				i = 6
				while calculateTokensInPieceOfText(preText) >100:
					i +=1
					preText = "".join(sectionFull[count1-1].split(". ")[::-1][0:7-i][::-1])
					print(i)
			else:
				preText = ""

			if calculateTokensInPieceOfText(HighLight)>200:
				HighLight = callGPT3AndOutputNewText(HighLight, "", highlight = "", Ttype = "sum1", max_tokens = 150) + '\n'
			OutPutText += callGPT3AndOutputNewText(section, preText, highlight = HighLight, Ttype = "sum1", max_tokens = 600) + '\n'
			HighLight += callGPT3AndOutputNewText(section, preText, highlight = "", Ttype = "highlight", max_tokens = 80)	
				# while count <2: #or calculateTokensInPieceOfText(section)> max_tokens-200:
				# 	if count == 0:
				# 		Prompt = 'This is a part or chapter of a scientific article, you are preparing it for summarization, please clean up any unnecessary punctuation, remove references and generally reduce the amount of tokens in this text without changing the meaning'
	
	print("\n output \n ")
	print(OutPutText)
	print("\n highlight \n ")
	print(HighLight)
	saveOutputTextFromGPT(OutPutText)
	saveHighlightTextFromGPT(HighLight)
				# 	if count == 1:
				# 		prompt = "" 
				# 	sectionLength = calculateTokensInPieceOfText(section)
				# 	lengthDividedByMaxLength = sectionLength/ max_tokens
				# 	if math.ceil(lengthDividedByMaxLength + MagicVarForAddingToDivider) > 1:
				# 		amountOfPartsToCutUpText = math.ceil(lengthDividedByMaxLength + MagicVarForAddingToDivider)

				# 	count +=1


	### Determine all sections and their length IN ORDER
	### Deterimine total lenght
	### Determine amount of tokens for each
	### Determine amount of space left after the summary output is being filled
	### Chop up article using specific formula
	### Provide 50 previous tokens if of same section 
	### Per section
	### send text to chatgpt
		### Different prompts for different things
		### Write the same text in less space, fix interpunction, and write it in 80% of the size, make text conciser without losing meaning
		### If a chapter requires several text prompts, include 200 token review/highlight, use for next part
		### Summarize this text to 80% the size
		### Summarize to 80% of the size

	### check response size > adjust token/chopping formula
	### Go to next section


	return text

def saveOutputTextFromGPT(text):
	try:
		with open('GPTOutput.txt', 'w',encoding='utf8') as f:			
			f.write(text)
	except Exception as e:
		print("GPTOutput.Txt was open, could not edit \n full error: %s"%e)

def saveHighlightTextFromGPT(text):
	try:
		with open('GPTHighlight.txt', 'w',encoding='utf8') as f:
			f.write(text)
	except Exception as e:
		print("GPTHighlight.Txt was open, could not edit \n full error: %s"%e)



def splitTextToSections(text):
	### Find <<< >>> at beginning of each text section to identify it, we added this tag in the access XML function 
	sections = re.split(r'<<<.*?>>>', text)[1:]
	return sections


def calculateTokensInPieceOfText(text): # should be parsed in a section; returns tokens
	encoding = tiktoken.encoding_for_model('text-curie-001')
	"""Returns the number of tokens in a text string."""
	amountOfTokensInText = len(encoding.encode(text))


	return amountOfTokensInText




# Function to split the text into chunks of a maximum number of tokens
def split_into_many(text, max_tokens = 600):

	#print(text)
	# Split the text into sentences
	sentences = text.split('. ')
	tokenizer = tiktoken.encoding_for_model('text-curie-001')
	n_tokens = []

	n_tokens = [len(tokenizer.encode(" " + sentence)) for sentence in sentences]
	#print('d')
	#for sentence in sentences:
		#n_tokens += [len(tokenizer.encode(" " + sentence))]

	# Get the number of tokens for each sentence
	tokenizer = tiktoken.encoding_for_model('text-curie-001')#text-davinci-003
	#n_tokens = [len(tokenizer.encode(" " + sentence)) for sentence in sentences]
	
	#n_tokens = [len(encoding.encode(" " + sentence)) for sentence in sentences]
	
	chunks = []
	tokens_so_far = 0
	chunk = []

    # Loop through the sentences and tokens joined together in a tuple
	for sentence, token in zip(sentences, n_tokens):
		if sentence != None:
        # If the number of tokens so far plus the number of tokens in the current sentence is greater 
        # than the max number of tokens, then add the chunk to the list of chunks and reset
        # the chunk and tokens so far
		#print(tokens_so_far)
		#print(type(sentence))
			if int(tokens_so_far) + int(token) > int(max_tokens):
				chunks.append(". ".join(chunk) + ".")
				chunk = []
				tokens_so_far = 0
				#print('ddd')
			#print('d')
	        # If the number of tokens in the current sentence is greater than the max number of 
	        # tokens, go to the next sentence
			if token > max_tokens:
				continue

	        # Otherwise, add the sentence to the chunk and add the number of tokens to the total

			chunk.append(sentence)
			tokens_so_far += token + 1
	if chunks == []:
		chunks.append(". ".join(chunk) + ".")

	return chunks
    



def main():
	global local_vars
	nameOfRun = "PMCID_test"
	path = "E:/python/Webscraper/"
	os.chdir(path)
	createDirectories(path)
	#### Always On
	SearchDateAndQueryDict = readDictionaryFromFile(path)
	Currentdate = setCurrentDate()
	QueryTerms = readQueryTermsFile(path)
	df = loadDataFrame(path)

	#### Perform pubmedSearch
	# df = performSearch(df,SearchDateAndQueryDict,QueryTerms,Currentdate,path)
	# os.chdir(path)	
	# df = convertPMIDToPMCID(df,path)

	# ### Saving code
	# saveCommaSeperatedListOf_PMIDs(df,QueryTerms)
	# saveCommaSeperatedListOf_PMCIDs(df,QueryTerms)
	# saveDictionaryToFile(SearchDateAndQueryDict)
	# saveDataFrame(df,path)

	### PyGetPapers, gets papers found in DF 
	#runPyGetPapersPerQueryTerm(path,QueryTerms,nameOfRun)

	### should run for each file
	openAndProcessXMLsForAllQueryTerms(QueryTerms,path,nameOfRun)
	openTextFilesForProcessing(QueryTerms,path,nameOfRun)

	#calculateTokensInPieceOfText()




	#local_vars = inspect.currentframe().f_locals
if __name__ == "__main__":
	main()