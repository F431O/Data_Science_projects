from mrjob.job import MRJob

class MRQuery1Job(MRJob):
	def mapper(self, _, line):
			#elimino la riga si intestazione
            if line[0] == "s":
                return
            
            #prendo i vari campi separati dal ;
            fields = line.split(";")
            sensorId = fields[0]
            temperature = fields[-1]
            
            #se i campi sensorId o temperature sono vuoti skippa la linea
            if sensorId == "" or temperature == "":
            	return
            
            #trasformo temperature in un float in modo da poterlo comparare	
            temperature = float(temperature)
            #butto le righe con delle tempirature anomale (si è scelto di usare -50 come temperatura estremamente fredda e +70 come esrememente calda)
            if temperature > 70 or temperature < -50:
            	return
            yield (sensorId, temperature)

	def reducer(self, key, value):
		#trasformo i valori in una lista in modo da poter usare la funzione len
		values = list(value)
		sum_values = sum(values)
		lung_values = len(values)
		
		#calcolo la media, ovviamente controllo che il valore di lunghezza non sia 0 per evitare di dividere per 0, 
		#in caso sia 0 setto di defolt il valore della media a 0
		if lung_values != 0:
			avarage = sum_values / lung_values
		else:
			avarage = 0
		
		yield (key, avarage)

if __name__ == '__main__':
	MRQuery1Job.run()
