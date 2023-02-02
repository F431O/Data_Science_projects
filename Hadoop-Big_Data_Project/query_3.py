from mrjob.job import MRJob

class Query3job(MRJob):
	def mapper(self, _, line):
		#elimino la riga si intestazione
		if line[0] == "s":
			return
			
		#prendo i vari campi separati dal ;
		fields = line.split(";")
		temperature = fields[-1]
		#il campo timestamp è formato da giorno e ora, in questo modo prendo solamente il giorno
		day = fields[5].split("T")[0]
		latitude = fields[3]
		longitude = fields[4]
		delta = 0.1
		lat0 = -40.0
		lon0 = -120.0
		
		#se i campi temperature o day o latitude o longitude sono vuoti skippa la linea
		if temperature == "" or day == "" or latitude == "" or longitude == "":
			return
		
		#trasformo temperature, latitude e longitude in un float in modo da poterli comparare
		temperature = float(temperature)
		latitude = float(latitude)
		longitude = float(latitude)
		
		#setto i range accettabili di temperatura, latitudine e longitdine
		if temperature < -50 or temperature > 70:
			return
		
		if latitude < -90 or latitude > 90:
			return
		
		if longitude < -180 or longitude > 180:
			return
		
		#costruisco x e y come richiesto dall'esercizio
		x = int((latitude - lat0)/delta)
		y = int((longitude - lon0)/delta)
		
		#restituisco come chiave la coppia x e y che rappresentano le aree delle zone e come valori la coppia temperatura giorno
		yield ((x, y),(temperature, day))

	def reducer(self, key, values):
		#setto la temperatura massima fuori scala
		max_temp = -60
	
		#trovo il giorno con la temperatura massima
		for temp,day in values:
			if temp > max_temp:
				max_temp = temp
				day_max = day
		
		#il risultato avra per ogni area il giorno con la temperatura massima
		yield (key, day_max)

if __name__ == '__main__':
	Query3job.run()
