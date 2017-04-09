# -*- coding: utf-8 -*-
"""
Created on Fri Apr 07 01:20:29 2017
Find the water crashed with protein in pdb.
@author: amseram
"""
from biopandas.pdb import PandasPDB as pdb
import sys,os,numpy as np
from numpy.linalg import norm 
'''from multiprocessing.dummy import Pool'''

class atom:
    def __init__(self,atom_info):
        self.x = atom_info[11]
        self.y = atom_info[12]
        self.z = atom_info[13]
        self.atom_element = atom_info[-3]
        self.atom_number = atom_info[1]
        self.residue_number = atom_info[8]
        self.residue_name = atom_info[5]
        self.check_crash = False
        self.closest_atom = ""
        pass
    def __str__(self):
        return "%s : %s | %s : %s |"%(tuple(self.atom_element,self.atom_number,self.residue_name,self.residue_number))
        pass
    def get_distance(self,atom):
        return norm(np.array([self.x,self.y,self.z]) - np.array([atom.x,atom.y,atom.z]))
        pass
    def get_closest(self,atoms,criterion=0.5):
        '''
        First step: Finding the closest atom for each water molercules
        Second setp:Calc the distance between them>> if distance shoter than "criterion" this water will be marked.
        '''
        def check_atom(closest_atom_info,current_atom):
            total_length = abs(current_atom.x-self.x) + abs(current_atom.y - self.y) + abs(current_atom.z - self.z)
            if total_length < closest_atom_info[-1]:
                closest_atom_info[-1] = total_length
                closest_atom_info[0] = current_atom
                pass
            return closest_atom_info
        closest_atom = reduce(check_atom,atoms.atoms,[atoms.atoms[0],999.0])[0]
        self.closest_atom = closest_atom
        if self.get_distance(closest_atom) < float(criterion):
            self.check_crash = True
            pass
        pass
    pass 

class atoms:
    def __init__(self):
        self.atoms = []
        pass
    def __str__(self):
        atoms_string = [str(atom) for atom in self.atoms]
        return "\n".join(atoms_string)
    def add_atom(self, atom):
        self.atoms.append(atom)
        pass
    pass

class crash_water:
    def __init__(self):
        self.atoms = []
        pass
    def add_crash_water(self,crash_water):
        self.atoms.append(crash_water)
        pass
    def gen_content(self):
        lines = [str(atom) for atom in self.atoms]
        return "\n".join(lines)
        pass
    pass

def read_from_pdb(pdb_name):
    pdb_object = pdb().read_pdb(pdb_name)
    all_atoms = pdb_object.df['ATOM'].values
    all_atoms = np.concatenate((all_atoms,pdb_object.df['HETATM'].values))
    waters = atoms()
    protein = atoms() 
    def choose_water(current_atom):
        tmp_atom_object = atom(current_atom)
        if 'WAT' == tmp_atom_object.residue_name :
            if tmp_atom_object.atom_element == 'O':
                waters.add_atom(tmp_atom_object)
                pass
        else:
            protein.add_atom(tmp_atom_object)
            pass
        pass
    map(choose_water,all_atoms)
    return waters,protein

def print_to_file(lines):
    file_object = open('report.out','w')
    print >>file_object, lines
    pass

def main(pdb_name,criterion):
    waters, protein = read_from_pdb(pdb_name)
    def check_waters(water):
        water.get_closest(protein,criterion)
        pass
    map(check_waters,waters.atoms)
    crashed_water = crash_water()
    for water in waters.atoms:
        if water.check_crash :
            crashed_water.add_crash_water(water)
            pass
        pass
    elimed_waters = crashed_water.gen_content()
    print_to_file(elimed_waters)
    pass
    
if __name__=='__main__':
    def get_pdb():
        pdb_name = ""
        while pdb_name == "":
            pdb_name = str(raw_input('PDB : '))
            pass
        return pdb_name
    if len(sys.argv) > 1:
        pdb_name = sys.argv[1]
        if len(sys.argv) > 2 :
            criterion = float(sys.argv[-1])
            pass
    else:
        print "Please input the pdb name:"
        pdb_name = get_pdb()
        criterion = float(0.5)
        pass
    if not ".pdb" in pdb_name :
        pdb_name = str(pdb_name) + ".pdb"
        pass
    if os.path.isfile(pdb_name):
        main(pdb_name,criterion)
    else:
        print "Here is no %s. Plz check your pdb!"%pdb_name
        pass
    pass
