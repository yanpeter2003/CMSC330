class Roster
  def initialize
    @roster = []
    @size = 0
    return self
  end

  def add(person)
    @roster.push(person)
    @size += 1
    return nil
  end

  def size
    return @size
  end

  def remove(person)
    @roster.delete(person)
    @size -= 1
    return nil
  end

  def getPerson(name)
    return @roster.find{|person| person.getName == name}
  end

  def map
    @roster.each do |person|
      yield person
    end
  end
end

class Person

  def initialize(name, age)
    @name = name
    @age = age
    return self
  end

  def getAge
    return @age
  end

  def setAge(x)
    @age = x
    return self
  end

  #Helper method
  def getName
    return "#{@name}"
  end
  
end

class Student < Person
  def initialize(name,age, grade)
    @name = name
    @age = age
    @grade = grade
    return self
  end

  def getGrade
    return @grade
  end

  def changeGrade(x)
    @grade = x
  end
end

class Staff < Person
  def initialize(name,age, position)
    @name = name
    @age = age
    @position = position
    return self
  end

  def getPosition
    return @position
  end

  def changePosition(newPosition)
    @position = newPosition
  end
end
