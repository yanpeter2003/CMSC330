class Translator
  def initialize(words_file, grammar_file)
    file1 = File.open(words_file)
    file2 = File.open(grammar_file)
    # Check language file for proper structure
    @word_regex = /([-a-z]+), ([A-Z]{3}), ([A-Z]{1}[a-z0-9]+):([-a-z]+)+/
    @grammar_regex = /([A-Z]{1}[a-z0-9]+): ([A-Z]{3})(\{([1-9]+)\})?((, [A-Z]{3})(\{([1-9]+)\})?)+/
    word_line = file1.gets
    grammar_line = file2.gets
    # Array that stores Word objects, which contain their word, their POS, and their translations in other languages
    @dictionary = []
    # Hash that has languages as keys and their grammar structures as values
    @grammar_structures = {}

    while word_line
      # Check if line of words_file has correct structure
      if word_line =~ @word_regex
        eng_word = $1
        word_pos = $2
        languages = word_line.scan(/[A-Z]{1}[a-z]+/)
        words = word_line.scan(/(?<=:)[-a-z]+/)
        translations = Hash[languages.zip(words)]
      end
      @dictionary.push(Word.new(eng_word, word_pos, translations))
      word_line = file1.gets
    end

    while grammar_line
      # Check if line of grammar_file has correct structure
      if grammar_line =~ @grammar_regex
        language = $1
        #Array of two element arrays, with first element being POS and second being number of times POS repeats
        structure = grammar_line.scan(/(?<pos>[A-Z]{3})(\{(?<times>[1-9]+)\})?/)
        #Array that stores all POS as elements, including repeated ones
        newStructure = []
        structure.each do |pos, times|
          if times == nil
            newStructure.push(pos)
          else
            for i in 1..times.chomp.to_i do
              newStructure.push(pos)
            end
          end
        @grammar_structures[language] = newStructure
        end
      end
      grammar_line = file2.gets
    end
  end #End of initialize

  # Inner class to store information on word
  class Word
    #String, String, Hash
    def initialize(word, pos, translations)
      @word = word
      @pos = pos
      @translations = translations
    end #End of initialize

    #Helper function to retrieve word
    def getWord
      @word
    end
    
    #Helper function to retrieve POS
    def getPOS
      @pos
    end

    #Helper function to retrieve translations
    def getTranslations
      @translations
    end

    # Updates the translations of the Word object.
    def updateWord(newTranslations)
      @translations = newTranslations
    end #End of updateWord

    # Find translation of word in language
    def findTranslation(language)
      if language != "English"
        return @translations[language]
      else 
        return @word
      end
    end #End of findTranslation
  end #End of class Word

  # Find given word in the dictionary, or its English translation if it isn't an English word. Return nil if the word is not found
  def findWord(word)
    @dictionary.each do |i|
      if i.getWord == word
        return i
      else
        (i.getTranslations).each do |key, value|
          if value == word
            return i
          end
        end
      end
    end
    return nil
  end

  #Helper function to translate a word into English
  def untranslate(word, language)
    @dictionary.each do |i|
      if i.findTranslation(language) == word
        return i.getWord
      end
    end
    return nil
  end

  # part 1
  # Update the words in your lexicon, by reading in the file. If you have already seen a word, update your data structure so that any additional translations are added.
  def updateLexicon(inputfile)
    word_file = File.open(inputfile)
    # Check for proper structure
    word_line = word_file.gets
    while word_line
      if word_line =~ @word_regex
        # Initialize variables
        eng_word = $1
        word_pos = $2
        languages = word_line.scan(/[A-Z]{1}[a-z]+/)
        words = word_line.scan(/(?<=:)[-a-z]+/)
        translations = Hash[languages.zip(words)]
        # Find if word exists to update
        if findWord(eng_word) != nil
          # If the same word appears with the same POS, update the translations
          if findWord(eng_word).getPOS == word_pos
            findWord(eng_word).updateWord(translations)
          # If the same word appears with a different POS that was not seen before it is considered a new word with that POS.
          else
            @dictionary.push(Word.new(eng_word, word_pos, translations))
          end
        else
          # If word does not exist in dictionary, add it
          @dictionary.push(Word.new(eng_word, word_pos, translations))
        end
      end
      word_line = word_file.gets
    end
  end #End of updateLexicon
  
  # Update your grammar knowledge by reading in the file. If you have already seen a language's grammatical structure, update it with the new data.
  def updateGrammar(inputfile)
    grammar_file = File.open(inputfile)
    grammar_line = grammar_file.gets
    while grammar_line
      # Check each line for proper structure
      if grammar_line =~ @grammar_regex
        language = $1
        structure = grammar_line.scan(/[A-Z]{3}/)
        # Adds new language structure if does not exist, updates it otherwise
        @grammar_structures[language] = structure
      end
      grammar_line = grammar_file.gets
    end
  end

  # part 2
  #struct is either a grammar, or an array of POS. Given this structure, create a sentence in the given language that matches that structure. If you cannot, return nil. When multiple POS exist, you can choose any word that has that POS.
  def generateSentence(language, struct)
    sentence = []
    grammar_structure = []
    # If struct is a string, find its grammar structure
    if struct.class == String
      # Assign grammar_structure variable to array of POS corresponding to struct
      grammar_structure = @grammar_structures[struct]
      if grammar_structure == nil
        return nil
      else
        # Iterate through grammar structure
        grammar_structure.each do |pos|
          #Place array in loop to refresh for each POS
          valid_words = []
          # Iterate through dictionary and find words with corresponding POS, then find their translations and store them as valid words
          @dictionary.each do |i|
            if i.getPOS == pos and i.findTranslation(language) != nil
              valid_words.push(i)
            end
          end
          # Randomly choose one stored word to put in sentence if there are valid words
          if valid_words.length > 0
            sentence.push(valid_words.sample(1)[0].findTranslation(language))
          else
            return nil
          end
        end
      end
      
    # Else if struct is an array of POS, search dictionary for words to fill
    else
      struct.each do |pos|
        #Place array in loop to refresh for each POS
        valid_words = []
        @dictionary.each do |i|
          if i.getPOS == pos and i.findTranslation(language) != nil
            valid_words.push(i)
          end
        end
        # Randomly choose one stored word to put in sentence if there are valid words
        if valid_words.length > 0
          sentence.push(valid_words.sample(1)[0].findTranslation(language))
        else
          return nil
        end
      end
    end
    
    final_sentence = sentence.join(' ')
    return final_sentence
  end

  #language is a language name. Check if the sentence matches that language's grammatical structure and return true or false.
  def checkGrammar(sentence, language)
    grammar_structure1 = []
    grammar_structure2 = @grammar_structures[language]
    sentence.scan(/([-a-z]+)/).each do |x|
      if language == "English"
        #Get POS for each word in sentence
        grammar_structure1.push(findWord(x[0]).getPOS)
      else
        grammar_structure1.push(findWord(untranslate(x[0], language)).getPOS)
      end
    end
    #Compare grammar structure arrays
    return grammar_structure1.eql?(grammar_structure2)
  end

  # struct1 is either a language name, or an array of POS. struct2 is either a language name or a array of POS. Given a sentence and its structure (struct1), change the sentence to match struct2. This should work independently of that actual gramatical structure of sentence and if the words in sentence are actual words or not. You may assume sentence.length == struct1.length == struct2.length. When multiple POS exist, you can swap the order however you want as long as your resuling sentence has all the same words in the input. If you cannot change the structure, then return nil.
  def changeGrammar(sentence, struct1, struct2)
    if struct1.class == String
      structure1 = @grammar_structures[struct1]
    else
      structure1 = struct1
    end
    if struct2.class == String
      structure2 = @grammar_structures[struct2]
    else
      structure2 = struct2
    end
    newSentence = []
    #Iterate through sentence's words
    structure2.each do |pos2|
      #Iterate through struct2's structure 
      sentence.scan(/([-a-z]+)/).each do |x|
        #Iterate through each word in dictionary and their translations to find POS of each word in sentence. If it matches with current pos, add it to new sentence
        if findWord(x[0]).getPOS == pos2
          newSentence.push(x)
          #Break to prevent duplicate words being added (see test that expects el el camion)
          break
        end
      end #End of sentence.each
    end #End of structure2.each
    return newSentence.join(' ')
  end #End of changeGrammar

  # part 3
  # Given a sentence sentence that matched the gramatical structure of language1, convert the entire sentence into the target language, language2, keeping the same grammatical structure. If any part of the sentence cannot be translated, then return nil. Otherwise return the translated sentence. You may assume that each word in sentence is in language1.
  def changeLanguage(sentence, language1, language2)
    translatedSentence = []
    sentence.scan(/([-a-z]+)/).each do |x|
      #For each word in sentence, find its English translation. If it has no English version, it is not in the dictionary, return nil
      if untranslate(x[0], language1) != nil
        currentWord = untranslate(x[0], language1)
        #Find current word's translation in language2. If it has no translation, return nil
        if findWord(currentWord).findTranslation(language2) != nil
          #Add translated current word to new sentence array 
      translatedSentence.push(findWord(currentWord).findTranslation(language2))
        else
          return nil
        end
      else 
        return nil
      end
    end
    # Create string from array and return it
    return translatedSentence.join(' ')
  end #End of changeLanguage

  # Given a sentence that matches the gramtical structure of language1, convert the entire sentence into the target language, language2, and change the grammatical structure to match language2. If any part of the sentence cannot be translated, then return nil. Otherwise return the translated sentence.
  def translate(sentence, language1, language2)
    # Convert sentence into language2 and check for nil
    translated_sentence = changeLanguage(sentence, language1, language2)
    if translated_sentence != nil
      # Change the grammatical structure to match language2
      translated_sentence = changeGrammar(translated_sentence, language1, language2)
      #Check if translated sentence has same grammar structure as target language
      if checkGrammar(translated_sentence, language2) == true
        return translated_sentence
      else 
        return nil
      end
    end
    return nil
  end

end #End of class Translator
