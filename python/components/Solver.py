
import pygame
import sys
import os
from pygame.locals import *
import re
import tkinter as tk
from tkinter import filedialog
from integration.prolog_Bridge import PrologBridge
from components.Cinematic import Cinematic
from integration.Rules import Rules
from integration.Search import Search
from integration.SearchNoConstraints import SearchNoConstraints
from integration.State import StateManager
from integration.Contraints import Constraints
from integration.Facts import Facts
import json
import networkx as nx
import matplotlib.pyplot as plt
from matplotlib.backends.backend_agg import FigureCanvasAgg
import pygame
import io
import time  # Para controlar el tiempo de las imágenes
from assets.texts.constants import SCREEN_WIDTH, SCREEN_HEIGHT, BUTTON_WIDTH, BUTTON_HEIGHT, MARGIN, FONT_SIZE, SCROLL_SPEED
from assets.texts.colors import WHITE, BLACK, GRAY, LIGHT_GRAY, DARK_GRAY, RED, GREEN, BLUE, YELLOW, PURPLE

# Initialize P

class Solver:

    def __init__(self, bridge, add_output,):
        self.add_output = add_output
        self.bridge = bridge
        self.map = {}
        try:
            self.retro_font = pygame.font.Font(
                "retro_font.ttf", 24
            )  # Cambia "retro_font.ttf" por tu fuente
            self.retro_font_small = pygame.font.Font("retro_font.ttf", 18)
        except:
            print("No se pudo cargar la fuente retro, usando fuente por defecto")
            self.retro_font = pygame.font.SysFont("Arial", 24)
            self.retro_font_small = pygame.font.SysFont("Arial", 18)
        self.font = self.retro_font
        self.small_font = self.retro_font_small


    def find_solution(self):
        """Find escape solution"""
        self.add_output("\nSearching for escape solution with bfs...")
        solution = self.bridge.find_escape_plan()

        if solution:
            self.add_output("\nSolution found:", False, 80)
            for step in solution:
                self.add_output(f"- {step}", False, 80)
            self.add_output(f"Total steps required: {len(solution)}", False, 80)
        else:
            self.add_output("\nNo escape solution found! The room might be unsolvable.")

    def find_solution_no(self):
        """Find escape solution"""
        self.add_output("\nSearching for escape solution with no constraints...")
        solution = self.bridge.find_escape_plan_no()

        if solution:
            self.add_output("\nSolution found:", False, 80)
            for step in solution:
                self.add_output(f"- {step}", False, 80)
            self.add_output(f"Total steps required: {len(solution)}", False, 80)
        else:
            self.add_output("\nNo escape solution found! The room might be unsolvable.")

    def find_solution_start(self):
        """Find escape solution"""
        self.add_output("\nSearching for escape solution with A*...")
        solution = self.bridge.find_escape_plan_star()

        if solution:
            self.add_output("\nSolution found:", False, 80)
            for step in solution:
                self.add_output(f"- {step}", False, 80)
            self.add_output(f"Total steps required: {len(solution)}", False, 80)
        else:
            self.add_output("\nNo escape solution found! The room might be unsolvable.")
