#!/usr/bin/env python
# coding: utf-8

# In[5]:


import numpy as np
import matplotlib.pyplot as plt
import csv


# In[6]:


imported_data = []
with open('data.csv','r') as csvfile:
    data = csv.reader(csvfile)

    for row in data:
        imported_data.append(float(row[0]))


# In[10]:


imported_data = np.array(imported_data)
field = np.reshape(imported_data,(1000,1000))


# In[13]:


fig = plt.figure(figsize=(15.0,15.0), dpi=200.0, frameon=False)
ax = plt.Axes(fig, [0,0,1,1])
ax.set_axis_off()
fig.add_axes(ax)
ax.imshow(field / np.max(field), cmap='jet', vmin=0, vmax=1)
plt.savefig("test.eps")
plt.show()


# In[ ]:




