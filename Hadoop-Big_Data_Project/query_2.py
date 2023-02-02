from mrjob.job import MRJob

class Query2job(MRJob):
	def mapper(self, _, line):
		#elimino la riga si intestazione
		if line[0] == "s":
			return
		
		#prendo i vari campi separati dal ;
		fields = line.split(";")
		sensorId = fields[0]
		temperature = fields[-1]
		#il campo timestamp è formato da giorno e ora, in questo modo prendo solamente il giorno
		day = fields[5].split("T")[0]
		pressure = fields[6]
		
		#se i campi sensorId o temperature o pressure o day sono vuoti skippa la linea
		if sensorId == "" or temperature == "" or pressure == "" or day == "":
			return
		
		#trasformo temperature e pressure in un float in modo da poterli comparare
		temperature = float(temperature)
		pressure = float(pressure)
		
		#i valori massimi e minimi di temperatura e pressione sono stati sceli facendo una ricerca sul web
		if(temperature < -50 or temperature > 70) or (pressure > 104500 or pressure < 98000) :
			return
		
		#restituisco come chiave la coppia sensorId giorno e come valore la coppia temperatura pressione
		yield ((sensorId, day),(temperature, pressure))

	def reducer(self, key, values):
		#setto i valori minimi e massimi di temperatura e pressione in moda da essere fuori scala cosicche i valori massimi e minimi effettivi siano veritieri
		max_temp = -60
		max_press = -1000
		min_temp = 80
		min_press = 1000000
		
		#trovo il minimo e massimo della temperatura e della pressione
		for temp,press in values:
			if temp > max_temp:
				max_temp = temp
			if temp < min_temp:
				min_temp = temp
			if press > max_press:
				max_press = press
			if press < min_press:
				min_press = press
		
		#il risultato avrà per ogni sensorId-giorno la pressione massima e minima e la temperatura massima e minima
		yield (key, (max_press, min_press, max_temp, min_temp))

if __name__ == '__main__':
	Query2job.run()
